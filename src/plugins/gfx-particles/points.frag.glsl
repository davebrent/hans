#version 430

in vec4 v_color;
out vec4 color;

void main(){
  color = v_color;
  color.a = 1.0;
}
