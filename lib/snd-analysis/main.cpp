#include <aubio/aubio.h>
#include "hans/engine/object.hpp"

#define REAL_BUFF_NAME 0xf3b756f37cabf321  /* snd/fft/real */
#define IMAG_BUFF_NAME 0x716e5449ebf816b9  /* snd/fft/imag */
#define SIGN_BUFF_NAME 0xbd100e8a2d401c4e  /* snd/ifft/signal */
#define PARAM_METHOD 0x5d2dfaed5ad5f81b    /* method */
#define METHOD_CENTROID 0x56ae04ce70d4f343 /* centroid */
#define METHOD_ROLLOFF 0x7e13c94680db0c7f  /* rolloff */

using namespace hans;
using namespace hans::engine;

struct FFTState {
  Register inlet;
  Register outlet_real;
  Register outlet_imag;
  audio::Buffer buffer_real;
  audio::Buffer buffer_imag;

  aubio_fft_t* fft;
  fvec_t* compspec;
  fvec_t* in;
  cvec_t* spectrum;
  fvec_t* real;
  fvec_t* imag;
};

class FFTObject : protected AudioObject {
  friend class hans::engine::LibraryManager;

 public:
  using AudioObject::AudioObject;
  ~FFTObject();
  virtual void create(IPatcher& patcher) override;
  virtual void setup(Engine& engine) override;
  virtual void callback(Engine& engine) override;

 private:
  FFTState state;
};

FFTObject::~FFTObject() {
  if (state.fft != nullptr) {
    del_aubio_fft(state.fft);
    del_fvec(state.compspec);
    del_fvec(state.in);
    del_cvec(state.spectrum);
    del_fvec(state.real);
    del_fvec(state.imag);
  }
}

void FFTObject::create(IPatcher& patcher) {
  patcher.request(IPatcher::Resources::INLET, 1);
  patcher.request(IPatcher::Resources::OUTLET, 2);
}

void FFTObject::setup(Engine& engine) {
  state.inlet = engine.registers.make(id, Register::Types::INLET, 0);
  state.outlet_real = engine.registers.make(id, Register::Types::OUTLET, 0);
  state.outlet_imag = engine.registers.make(id, Register::Types::OUTLET, 1);
  state.buffer_real = engine.audio_buffers.make(id, REAL_BUFF_NAME);
  state.buffer_imag = engine.audio_buffers.make(id, IMAG_BUFF_NAME);

  auto winsize = state.buffer_real.size;
  state.fft = new_aubio_fft(winsize);
  state.compspec = new_fvec(winsize);
  state.in = new_fvec(winsize);
  state.spectrum = new_cvec(winsize);
  state.real = new_fvec(winsize);
  state.imag = new_fvec(winsize);
}

void FFTObject::callback(Engine& engine) {
  const auto input = engine.registers.read(state.inlet);
  const auto samples = static_cast<audio::sample*>(input);

  auto original = state.in->data;
  state.in->data = samples;
  aubio_fft_do_complex(state.fft, state.in, state.compspec);
  state.in->data = original;

  aubio_fft_get_spectrum(state.compspec, state.spectrum);
  aubio_fft_get_real(state.spectrum, state.real);
  aubio_fft_get_imag(state.spectrum, state.imag);

  auto real_buff = engine.audio_buffers.get(state.buffer_real, 0);
  auto imag_buff = engine.audio_buffers.get(state.buffer_imag, 0);

  for (auto i = 0; i < engine.config.blocksize; ++i) {
    real_buff[i] = state.real->data[i];
    imag_buff[i] = state.imag->data[i];
  }

  engine.registers.write(state.outlet_real, real_buff);
  engine.registers.write(state.outlet_imag, imag_buff);
}

static void realimag_to_compspec(size_t winsize, audio::sample* real,
                                 audio::sample* imag, fvec_t* compspec) {
  auto fftsize = winsize / 2 + 1;
  compspec->data[0] = real[0];
  compspec->data[winsize - 1] = imag[winsize - 1];
  for (auto i = 1; i < fftsize - 1; i++) {
    compspec->data[i] = real[i];
    compspec->data[winsize - i] = imag[winsize - i];
  }
  compspec->data[fftsize - 1] = real[fftsize - 1];
}

struct IFFTState {
  Register real;
  Register imag;
  Register outlet;
  audio::Buffer signal;
  aubio_fft_t* fft;
  cvec_t* fftgrain;
  fvec_t* out;
  fvec_t* compspec;
};

class IFFTObject : protected AudioObject {
  friend class hans::engine::LibraryManager;

 public:
  using AudioObject::AudioObject;
  ~IFFTObject();
  virtual void create(IPatcher& patcher) override;
  virtual void setup(Engine& engine) override;
  virtual void callback(Engine& engine) override;

 private:
  IFFTState state;
};

IFFTObject::~IFFTObject() {
  if (state.fft != nullptr) {
    del_aubio_fft(state.fft);
    del_cvec(state.fftgrain);
    del_fvec(state.out);
    del_fvec(state.compspec);
  }
}

void IFFTObject::create(IPatcher& patcher) {
  patcher.request(IPatcher::Resources::INLET, 2);
  patcher.request(IPatcher::Resources::OUTLET, 1);
}

void IFFTObject::setup(Engine& engine) {
  state.real = engine.registers.make(id, Register::Types::INLET, 0);
  state.imag = engine.registers.make(id, Register::Types::INLET, 1);
  state.outlet = engine.registers.make(id, Register::Types::OUTLET, 0);
  state.signal = engine.audio_buffers.make(id, SIGN_BUFF_NAME);

  state.fft = new_aubio_fft(state.signal.size);
  state.fftgrain = new_cvec(state.signal.size);
  state.out = new_fvec(state.signal.size);
  state.compspec = new_fvec(state.signal.size);
}

void IFFTObject::callback(Engine& engine) {
  const auto real =
      static_cast<audio::sample*>(engine.registers.read(state.real));
  const auto imag =
      static_cast<audio::sample*>(engine.registers.read(state.imag));

  realimag_to_compspec(engine.config.blocksize, real, imag, state.compspec);
  aubio_fft_rdo_complex(state.fft, state.compspec, state.out);
  engine.registers.write(state.outlet, state.out->data);
}

struct FeatureState {
  Parameter parameter;
  Register signal;
  Register outlet;
  hash method;
  fvec_t* value;
  fvec_t* in;
  cvec_t* fftgrain;
  aubio_pvoc_t* pvoc;
  aubio_specdesc_t* desc;
};

class FeatureObject : protected AudioObject {
  friend class hans::engine::LibraryManager;

 public:
  using AudioObject::AudioObject;
  ~FeatureObject();
  virtual void create(IPatcher& patcher) override;
  virtual void setup(Engine& engine) override;
  virtual void callback(Engine& engine) override;

 private:
  FeatureState state;
};

FeatureObject::~FeatureObject() {
  if (state.desc != nullptr) {
    del_fvec(state.value);
    del_fvec(state.in);
    del_cvec(state.fftgrain);
    del_aubio_pvoc(state.pvoc);
    del_aubio_specdesc(state.desc);
  }
}

void FeatureObject::create(IPatcher& patcher) {
  patcher.request(IPatcher::Resources::INLET, 1);
  patcher.request(IPatcher::Resources::OUTLET, 1);
  for (const auto& arg : patcher.arguments()) {
    if (arg.name == PARAM_METHOD && arg.type == Argument::Types::STRING) {
      state.method = arg.string;
    }
  }
}

void FeatureObject::setup(Engine& engine) {
  state.parameter = engine.parameters.make(id, state.method);
  state.signal = engine.registers.make(id, Register::Types::INLET, 0);
  state.outlet = engine.registers.make(id, Register::Types::OUTLET, 1);

  auto winsize = engine.config.blocksize;
  state.value = new_fvec(1);
  state.in = new_fvec(winsize);
  state.fftgrain = new_cvec(winsize);

  auto method = engine.strings.lookup(state.method);
  state.desc = new_aubio_specdesc(const_cast<char*>(method), winsize);
  state.pvoc = new_aubio_pvoc(winsize, winsize);
}

void FeatureObject::callback(Engine& engine) {
  const auto sig =
      static_cast<audio::sample*>(engine.registers.read(state.signal));

  auto in = state.in->data;
  state.in->data = sig;
  aubio_pvoc_do(state.pvoc, state.in, state.fftgrain);
  state.in->data = in;
  aubio_specdesc_do(state.desc, state.fftgrain, state.value);

  if (state.method == METHOD_CENTROID || state.method == METHOD_ROLLOFF) {
    auto winsize = engine.config.blocksize;
    auto samplerate = engine.config.samplerate;
    auto hz = aubio_bintofreq(state.value->data[0], samplerate, winsize);
    engine.parameters.set(state.parameter, 0, hz);
  } else {
    engine.parameters.set(state.parameter, 0, state.value->data[0]);
  }

  engine.registers.write(state.outlet, sig);
}

HANS_PLUGIN_INIT(LibraryManager* library) {
  library->add_object<FFTState, FFTObject>("snd-fft");
  library->add_object<IFFTState, IFFTObject>("snd-ifft");
  library->add_object<FeatureState, FeatureObject>("snd-feature");
}
