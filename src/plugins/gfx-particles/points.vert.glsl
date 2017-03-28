#version 430

in float position_x;
in float position_y;

in float color_r;
in float color_g;
in float color_b;
in float color_a;

out vec4 v_color;

void main () {
  gl_Position = vec4(position_x, position_y, 0.0, 1.0);
  v_color = vec4(color_r, color_g, color_b, color_a);
}
