#version 330 core

in float index;
in vec2 sample;

uniform float audio_buffer_length;

void main () {
  float x = (sample.x * -1.0) + sample.y;
  float y = sample.x + sample.y;
  gl_Position = vec4(x, y, 1.0, 1.0);
}
