#version 330 core

in float opacity;
out vec4 color;

void main(){
  color = vec4(opacity, 1.0, 1.0, 1.0);
  color.a = opacity;
}
