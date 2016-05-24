#include <math.h>
#include <cassert>
#include <cstdlib>
#include "./snd.loadfile_generated.h"
#include "hans/engine/object.hpp"

extern "C" {
#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
}

using namespace hans;

typedef struct { hans_hash filepath; } hans_loadsf_data;

void hans_loadsf_setup(hans_audio_object* self, hans_object_api* api) {
  hans_loadsf_data* data = static_cast<hans_loadsf_data*>(self->data);
  const char* filepath = api->strings->lookup(data->filepath);

  AVCodecContext* codec_ctx = nullptr;
  AVFormatContext* format_ctx = NULL;
  AVFrame* frame = nullptr;

  int err = avformat_open_input(&format_ctx, filepath, nullptr, nullptr);

  if (err) {
    api->logger->log(common::Logger::ERROR, "Unable to open input file");
    return;
  }

  err = avformat_find_stream_info(format_ctx, nullptr);

  if (err < 0) {
    api->logger->log(common::Logger::ERROR,
                     "Unable to read stream information");
    return;
  }

  int audio_stream = -1;
  for (int i = 0; i < format_ctx->nb_streams; i++) {
    if (format_ctx->streams[i]->codec->codec_type == AVMEDIA_TYPE_AUDIO) {
      audio_stream = i;
      break;
    }
  }

  if (audio_stream == -1) {
    api->logger->log(common::Logger::ERROR, "Unable to find stream");
    return;
  }

  AVCodecContext* codec_ctx_orig = format_ctx->streams[audio_stream]->codec;
  AVCodec* codec = avcodec_find_decoder(codec_ctx_orig->codec_id);

  if (codec == nullptr) {
    api->logger->log(common::Logger::ERROR, "Codec not supported");
    return;
  }

  codec_ctx = avcodec_alloc_context3(codec);

  if (avcodec_copy_context(codec_ctx, codec_ctx_orig) != 0) {
    api->logger->log(common::Logger::ERROR, "Unable to copy codec context");
    return;
  }

  if (avcodec_open2(codec_ctx, codec, nullptr) < 0) {
    api->logger->log(common::Logger::ERROR, "Unable to open codec");
    return;
  }

  if (codec_ctx->sample_fmt != AV_SAMPLE_FMT_S16) {
    api->logger->log(common::Logger::ERROR,
                     "Unsupported sample format (not 16 signed)");
    return;
  }

  if (av_sample_fmt_is_planar(codec_ctx->sample_fmt) == 1) {
    api->logger->log(common::Logger::ERROR,
                     "Unsupported sample format (planar)");
    return;
  }

  // Create the hans audio buffer
  unsigned channels_len = codec_ctx->channels;
  unsigned samples_len = format_ctx->streams[audio_stream]->duration;
  hans_audio_buffer* audio_buffer =
      api->audio_buffers->create(channels_len, samples_len, 1);

  // Iterate through the stream
  AVPacket packet;
  av_init_packet(&packet);
  frame = av_frame_alloc();

  int sample_pos = 0;

  while (av_read_frame(format_ctx, &packet) == 0) {
    if (packet.stream_index != audio_stream) {
      continue;
    }

    // Try to decode the packet into a frame
    int has_frame = 0;
    int read = avcodec_decode_audio4(codec_ctx, frame, &has_frame, &packet);

    if (read < 0) {
      api->logger->log(common::Logger::ERROR, "Failed to decode");
      return;
    }

    if (has_frame) {
      if (frame->sample_rate != 44100) {
        api->logger->log(common::Logger::ERROR, "Unsupported sample rate");
        return;
      }

      // Assumes interleaved data
      for (int i = 0; i < frame->nb_samples; ++i) {
        for (int c = 0; c < audio_buffer->channels; ++c) {
          audio_buffer->samples[c][sample_pos] = frame->data[0][i] / 65535;
        }

        sample_pos += 1;
      }

      // Unreference this frames own data
      av_frame_unref(frame);
    }

    av_free_packet(&packet);
  }

  avcodec_close(codec_ctx);
  av_free(codec_ctx);
  av_frame_free(&frame);
}

void hans_loadsf_new(hans_constructor_api* api, void* buffer, size_t size) {
  hans_audio_object* object = static_cast<hans_audio_object*>(buffer);
  object->setup = hans_loadsf_setup;
  object->callback = nullptr;

  void* offset = static_cast<char*>(buffer) + sizeof(hans_audio_object);
  hans_loadsf_data* data = static_cast<hans_loadsf_data*>(offset);

  av_register_all();

  api->request_resource(api, HANS_OUTLET, 1);
  auto arguments = api->get_args(api);
  for (int i = 0; i < arguments.length; ++i) {
    if (arguments.data[i].type == HANS_STRING &&
        arguments.data[i].name == LOADSF_ARG_FILEPATH) {
      data->filepath = arguments.data[i].string;
      break;
    }
  }

  object->data = data;
}

extern "C" {
void setup(hans_library_api* api) {
  api->register_object(api, "snd-loadfile",
                       sizeof(hans_audio_object) + sizeof(hans_loadsf_data),
                       hans_loadsf_new, nullptr);
}
}
