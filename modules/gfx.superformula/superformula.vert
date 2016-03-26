#version 330 core

#define EPSILON 1e-30

in vec3 position;

out vec3 v_position;
out vec3 v_normal;

uniform mat4 model_view_matrix;
uniform mat4 projection_matrix;

uniform float superformula_scale;
uniform float superformula_segments;
uniform float superformula_seed;
uniform float superformula_deform;
uniform vec2 superformula_m;
uniform vec2 superformula_n1;
uniform vec2 superformula_n2;
uniform vec2 superformula_n3;
uniform vec2 superformula_a;
uniform vec2 superformula_b;
uniform vec2 superformula_u;
uniform vec2 superformula_v;


float rand(vec2 co) {
  return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453);
}


float superformula(float phi, float m, float n1, float n2, float n3, float a,
                   float b) {
  a = max(a, EPSILON);
  b = max(b, EPSILON);

  float mp = (m * phi) / 4.0;

  float t1 = cos(mp) * (1.0 / a);
  float t2 = sin(mp) * (1.0 / b);

  t1 = pow(abs(t1), n2);
  t2 = pow(abs(t2), n3);

  float r = abs(pow(t1 + t2, 1.0 / n1));

  if (r == 0.0) {
    return 0.0;
  }

  return 1.0 / r;
}

vec3 calculate_position(vec2 position) {
  float phi = position.x / superformula_segments;
  float theta = position.y / superformula_segments;

  theta = mix(superformula_u.y, superformula_u.x, theta);
  phi = mix(superformula_v.y, superformula_v.x, phi);

  float r1 = superformula(theta, superformula_m.x, superformula_n1.x,
                          superformula_n2.x, superformula_n3.x,
                          superformula_a.x, superformula_b.x);
  float r2 = superformula(phi, superformula_m.y, superformula_n1.y,
                          superformula_n2.y, superformula_n3.y,
                          superformula_a.y, superformula_b.y);

  return vec3(
    r1 * cos(theta) * r2 * cos(phi),
    r1 * sin(theta) * r2 * cos(phi),
    r2 * sin(phi)
  );
}

vec3 calculate_face_normal(vec3 p1, vec3 p2, vec3 p3, vec3 p4) {
  vec3 v1 = p2 - p1;
  vec3 v2 = p4 - p2;
  return normalize(cross(v1, v2));
}

vec2 wrap_around(vec2 grid) {
  float segments = superformula_segments;

  // Make sure the grid position does not go out of bounds to the expected
  // number of segments
  grid.x = mix(grid.x, segments - abs(grid.x), float(grid.x < 0.0));
  grid.y = mix(grid.y, segments - abs(grid.y), float(grid.y < 0.0));
  grid.x = mix(grid.x, grid.x - segments, float(grid.x > segments));
  grid.y = mix(grid.y, grid.y - segments, float(grid.x > segments));

  return vec2(grid.x, grid.y);
}

vec3 calculate_vertex_normal(vec3 p0, vec2 grid) {
  //  1----2
  //  | \f1| \
  //  |f2\ |f6\
  //  3----x----4
  //    \f3| \f5|
  //     \ |f4\ |
  //       5----6
  vec2 v1 = wrap_around(vec2(grid.x - 1, grid.y - 1));
  vec2 v2 = wrap_around(vec2(grid.x,     grid.y - 1));
  vec2 v3 = wrap_around(vec2(grid.x - 1, grid.y));
  vec2 v4 = wrap_around(vec2(grid.x + 1, grid.y));
  vec2 v5 = wrap_around(vec2(grid.x,     grid.y + 1));
  vec2 v6 = wrap_around(vec2(grid.x + 1, grid.y + 1));

  vec3 p1 = calculate_position(v1);
  vec3 p2 = calculate_position(v2);
  vec3 p3 = calculate_position(v3);
  vec3 p4 = calculate_position(v4);
  vec3 p5 = calculate_position(v5);
  vec3 p6 = calculate_position(v6);

  vec3 f1 = calculate_face_normal(p0, p2, p0, p1);
  vec3 f2 = calculate_face_normal(p0, p1, p0, p3);
  vec3 f3 = calculate_face_normal(p0, p3, p0, p5);
  vec3 f4 = calculate_face_normal(p0, p5, p0, p6);
  vec3 f5 = calculate_face_normal(p0, p6, p0, p4);
  vec3 f6 = calculate_face_normal(p0, p4, p0, p2);

  return normalize(f1 + f2 + f3 + f4 + f5 + f6);
}

void main() {
  v_position = calculate_position(position.xy) * superformula_scale;
  v_normal = calculate_vertex_normal(v_position, position.xy);

  v_position.x += ((rand(v_position.xy * superformula_seed) * 2.0) - 1.0) * (
    superformula_seed * superformula_deform);
  v_position.y += ((rand(v_position.xz * superformula_seed) * 2.0) - 1.0) * (
    superformula_seed * superformula_deform);
  v_position.z += ((rand(v_position.yz * superformula_seed) * 2.0) - 1.0) * (
    superformula_seed * superformula_deform);

  gl_Position = projection_matrix * model_view_matrix * vec4(v_position, 1.0);
}
