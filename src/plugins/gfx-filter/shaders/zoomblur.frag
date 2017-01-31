#version 150

in vec2 uv;
out vec4 color;

uniform sampler2D u_texture;

// Resolution is the size of the input texture in pixels
uniform vec2 u_resolution;

// Center is the center of the input texture in pixels
uniform vec2 u_center;

// Amount of blur to apply (0-1)
uniform float u_amount;

float random (vec3 scale, float seed) {
  return fract(sin(dot(gl_FragCoord.xyz + seed, scale)) * 43758.5453 + seed);
}

void main() {
  vec4 original = texture(u_texture, uv);

  color = vec4(0.0);

  float amount = 0.6;

  float total = 0.0;
  vec2 toCenter = u_center - uv * u_resolution;
  float offset = random(vec3(12.9898, 78.233, 151.7182), 0.0);

  for(float t = 0.0; t <= 40.0; t++) {
    float perc = (t + offset) / 40.0;
    float weight = 4.0 * (perc - perc * perc);
    vec4 sample = texture(u_texture, uv + toCenter * perc * u_amount / u_resolution);
    sample.rgb *= sample.a;
    color += sample * weight;
    total += weight;
  }

  color = color / total;
  color.rgb /= color.a + 0.00001;
}
