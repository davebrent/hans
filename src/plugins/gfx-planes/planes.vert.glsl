#version 400

layout (location = 0) in vec2 position;
out vec2 uv;

uniform mat4 model_view_matrix;
uniform mat4 projection_matrix;
uniform float u_scale;

void main () {
  // gl_Position = vec4(position, 0.0, 1.0);
  //v_position = calculate_position(position.xy) * scale;
  //v_normal = calculate_vertex_normal(v_position, position.xy);
  uv = (position) + 0.5;

  float scale = u_scale;
  vec4 pos = vec4(position.x * scale, position.y * scale, 0.0, 1.0);
  gl_Position = projection_matrix * model_view_matrix * pos;
}
