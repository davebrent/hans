#version 330 core

in float index;
in float sample;
in float channel;

uniform float audio_buffer_length;

void main () {
  float t = ((index / audio_buffer_length) * 2.0) - 1.0;
  gl_Position = vec4(t, sample, 1.0, 1.0);
}
