#version 330 core

in vec2 uv;
out vec4 color;
uniform sampler2D u_texture;

void main(){
  color = texture(u_texture, uv);
}
