#version 330 core

in float id;
in vec2 position;

out float opacity;

void main () {
  gl_Position = vec4(position.x / 4.0, position.y / 3.0, 1.0, 1.0);
  opacity = (id / 100000.0) * 0.8;
}
