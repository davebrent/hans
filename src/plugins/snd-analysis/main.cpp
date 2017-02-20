#include <aubio/aubio.h>
#include <algorithm>
#include "hans/object.hpp"

#define REAL_BUFF_NAME 0xf3b756f37cabf321  /* snd/fft/real */
#define IMAG_BUFF_NAME 0x716e5449ebf816b9  /* snd/fft/imag */
#define SIGN_BUFF_NAME 0xbd100e8a2d401c4e  /* snd/ifft/signal */
#define PARAM_METHOD 0x5d2dfaed5ad5f81b    /* method */
#define METHOD_ENERGY 0x3f9a73b271805e09   /* energy */
#define METHOD_HFC 0x7da617bb1e8ad8c6      /* hfc */
#define METHOD_COMPLEX 0x5f6599e6ac9aa918  /* complex */
#define METHOD_PHASE 0x9c38c179d4311f91    /* phase */
#define METHOD_SPECDIFF 0x57cbc8bf661860c4 /* specdiff */
#define METHOD_KL 0xc8bc167a13da09c0       /* kl */
#define METHOD_MKL 0x296f16634dcc65c1      /* mkl */
#define METHOD_SPECFLUX 0xf10e9ed7bd4aa42d /* specflux */
#define METHOD_CENTROID 0x56ae04ce70d4f343 /* centroid */
#define METHOD_SPREAD 0xaefe2de866b71295   /* spread */
#define METHOD_SKEWNESS 0x35a598c181a4abcb /* skewness */
#define METHOD_KURTOSIS 0xcb87259c586798c0 /* kurtosis */
#define METHOD_SLOPE 0x91c70f31fa1831cb    /* slope */
#define METHOD_DECREASE 0x5366a8e4857da971 /* decrease */
#define METHOD_ROLLOFF 0x7e13c94680db0c7f  /* rolloff */

using namespace hans;

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

  template <class Archive>
  void serialize(Archive& ar) {
  }
};

class FFTObject : protected AudioObject {
  friend class hans::PluginManager;

 public:
  using AudioObject::AudioObject;
  ~FFTObject();
  virtual void create(IConfigurator& configurator) override;
  virtual void setup(context& ctx) override;
  virtual void update(context& ctx) override {
  }
  virtual void callback(context& ctx) override;

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

void FFTObject::create(IConfigurator& configurator) {
  configurator.request(IConfigurator::Resources::INLET, 1);
  configurator.request(IConfigurator::Resources::OUTLET, 2);
}

void FFTObject::setup(context& ctx) {
  state.inlet = ctx.registers.make(id, Register::Types::INLET, 0);
  state.outlet_real = ctx.registers.make(id, Register::Types::OUTLET, 0);
  state.outlet_imag = ctx.registers.make(id, Register::Types::OUTLET, 1);
  state.buffer_real = ctx.audio_buffers.make(id, REAL_BUFF_NAME);
  state.buffer_imag = ctx.audio_buffers.make(id, IMAG_BUFF_NAME);

  auto winsize = state.buffer_real.size;
  state.fft = new_aubio_fft(winsize);
  state.compspec = new_fvec(winsize);
  state.in = new_fvec(winsize);
  state.spectrum = new_cvec(winsize);
  state.real = new_fvec(winsize);
  state.imag = new_fvec(winsize);
}

void FFTObject::callback(context& ctx) {
  const auto samples = ctx.registers.read_block(state.inlet);

  auto original = state.in->data;
  state.in->data = samples;
  aubio_fft_do_complex(state.fft, state.in, state.compspec);
  state.in->data = original;

  aubio_fft_get_spectrum(state.compspec, state.spectrum);
  aubio_fft_get_real(state.spectrum, state.real);
  aubio_fft_get_imag(state.spectrum, state.imag);

  auto real_buff = ctx.audio_buffers.get(state.buffer_real, 0);
  auto imag_buff = ctx.audio_buffers.get(state.buffer_imag, 0);

  for (auto i = 0; i < ctx.settings.audio.blocksize; ++i) {
    real_buff[i] = state.real->data[i];
    imag_buff[i] = state.imag->data[i];
  }

  ctx.registers.write(state.outlet_real, real_buff);
  ctx.registers.write(state.outlet_imag, imag_buff);
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

  template <class Archive>
  void serialize(Archive& ar) {
  }
};

class IFFTObject : protected AudioObject {
  friend class hans::PluginManager;

 public:
  using AudioObject::AudioObject;
  ~IFFTObject();
  virtual void create(IConfigurator& configurator) override;
  virtual void setup(context& ctx) override;
  virtual void update(context& ctx) override {
  }
  virtual void callback(context& ctx) override;

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

void IFFTObject::create(IConfigurator& configurator) {
  configurator.request(IConfigurator::Resources::INLET, 2);
  configurator.request(IConfigurator::Resources::OUTLET, 1);
}

void IFFTObject::setup(context& ctx) {
  state.real = ctx.registers.make(id, Register::Types::INLET, 0);
  state.imag = ctx.registers.make(id, Register::Types::INLET, 1);
  state.outlet = ctx.registers.make(id, Register::Types::OUTLET, 0);
  state.signal = ctx.audio_buffers.make(id, SIGN_BUFF_NAME);

  state.fft = new_aubio_fft(state.signal.size);
  state.fftgrain = new_cvec(state.signal.size);
  state.out = new_fvec(state.signal.size);
  state.compspec = new_fvec(state.signal.size);
}

void IFFTObject::callback(context& ctx) {
  const auto real = ctx.registers.read_block(state.real);
  const auto imag = ctx.registers.read_block(state.imag);

  realimag_to_compspec(ctx.settings.audio.blocksize, real, imag,
                       state.compspec);
  aubio_fft_rdo_complex(state.fft, state.compspec, state.out);
  ctx.registers.write(state.outlet, state.out->data);
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

  template <class Archive>
  void serialize(Archive& ar) {
    ar(method);
  }
};

class FeatureObject : protected AudioObject {
  friend class hans::PluginManager;

 public:
  using AudioObject::AudioObject;
  ~FeatureObject();
  virtual void create(IConfigurator& configurator) override;
  virtual void setup(context& ctx) override;
  virtual void update(context& ctx) override;
  virtual void callback(context& ctx) override {
  }

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

const std::vector<hash> METHODS = {
    METHOD_ENERGY,   METHOD_HFC,      METHOD_COMPLEX,  METHOD_PHASE,
    METHOD_SPECDIFF, METHOD_KL,       METHOD_MKL,      METHOD_SPECFLUX,
    METHOD_CENTROID, METHOD_SPREAD,   METHOD_SKEWNESS, METHOD_KURTOSIS,
    METHOD_SLOPE,    METHOD_DECREASE, METHOD_ROLLOFF};

void FeatureObject::create(IConfigurator& configurator) {
  for (const auto& arg : configurator.arguments()) {
    if (arg.name == PARAM_METHOD && arg.type == Argument::Types::STRING) {
      state.method = arg.string;
    }
  }

  if (!state.method) {
    configurator.missing("method");
  } else {
    auto it = std::find(METHODS.begin(), METHODS.end(), state.method);
    if (it == METHODS.end()) {
      configurator.invalid("method");
    } else {
      configurator.request(IConfigurator::Resources::INLET, 1);
      configurator.request(IConfigurator::Resources::OUTLET, 1);
    }
  }
}

void FeatureObject::setup(context& ctx) {
  state.parameter = ctx.parameters.make(id, state.method);
  state.signal = ctx.registers.make(id, Register::Types::INLET, 0);
  state.outlet = ctx.registers.make(id, Register::Types::OUTLET, 1);

  auto winsize = ctx.settings.audio.blocksize;
  state.value = new_fvec(1);
  state.in = new_fvec(winsize);
  state.fftgrain = new_cvec(winsize);

  auto method = ctx.strings.lookup(state.method);
  state.desc = new_aubio_specdesc(const_cast<char*>(method), winsize);
  state.pvoc = new_aubio_pvoc(winsize, winsize);
}

void FeatureObject::update(context& ctx) {
  const auto sig = ctx.registers.read_block(state.signal);

  auto in = state.in->data;
  state.in->data = sig;
  aubio_pvoc_do(state.pvoc, state.in, state.fftgrain);
  state.in->data = in;
  aubio_specdesc_do(state.desc, state.fftgrain, state.value);

  if (state.method == METHOD_CENTROID || state.method == METHOD_ROLLOFF) {
    auto winsize = ctx.settings.audio.blocksize;
    auto samplerate = ctx.settings.audio.samplerate;
    auto hz = aubio_bintofreq(state.value->data[0], samplerate, winsize);
    ctx.parameters.set(state.parameter, 0, hz);
  } else {
    ctx.parameters.set(state.parameter, 0, state.value->data[0]);
  }

  ctx.registers.write(state.outlet, sig);
}

HANS_PLUGIN_INIT(PluginManager* manager) {
  manager->add_object<FFTState, FFTObject>("snd-fft");
  manager->add_object<IFFTState, IFFTObject>("snd-ifft");
  manager->add_object<FeatureState, FeatureObject>("snd-feature");
}
