#version 400

in vec2 uv;
out vec4 color;

subroutine vec4 FilterFunc(vec2 uv);
subroutine uniform FilterFunc FilterEffect;

uniform sampler2D image;
uniform vec2 screen_size;
uniform vec2 texture_size;
uniform vec2 amount;
uniform float weights[5];

subroutine(FilterFunc)
vec4 passthrough_filter(vec2 uv) {
  return texture(image, uv);
}

subroutine(FilterFunc)
vec4 gamma_filter(vec2 uv) {
  vec4 value = texture(image, uv);
  value.rgb = pow(value.rgb, vec3(1.0 / 2.2));
  return value;
}

subroutine(FilterFunc)
vec4 pixelate_filter(vec2 uv) {
  // amount ranges from resolution.x -> 0, the larger the number the
  // clearer the image.
  float pixel_size = 1.0 / amount.x;
  float aspect = screen_size.x / screen_size.y;

  // Round to the nearest interval of pixel_size
  uv.x = floor(uv.x / pixel_size) * pixel_size;
  pixel_size = aspect / amount.x;
  uv.y = floor(uv.y / pixel_size) * pixel_size;
  return texture(image, uv);
}

subroutine(FilterFunc)
vec4 greyscale_filter(vec2 uv) {
  vec3 luma = vec3(0.299, 0.587, 0.114);
  vec4 rgb = texture(image, uv);
  return vec4(vec3(dot(rgb.rgb, luma)), rgb.a);
}

subroutine(FilterFunc)
vec4 invert_filter(vec2 uv) {
  vec4 color = texture(image, uv);
  color.rgb = 1.0 - color.rgb;
  return color;
}

subroutine(FilterFunc)
vec4 rgb_to_yuv_filter(vec2 uv) {
  vec4 c = texture(image, uv);

  // ITU-R BT.709
  float Kr = 0.2126;
  float Kg = 0.587;
  float Kb = 0.0722;

  float yScale = 235.0 / 255.0;
  float yOffset = 16.0 / 255.0;
  float cScale = 240.0 / 255.0;
  float cOffset = 128.0 / 255.0;

  float y = ((Kr * c.r + Kg * c.g + Kb * c.b) * yScale) + yOffset;
  float Cb = ((-0.168736 * c.r - 0.331264 * c.g + 0.5 * c.b) * cScale) + cOffset;
  float Cr = ((0.5 * c.r - 0.418688 * c.g - 0.081312 * c.b) * cScale) + cOffset;
  return vec4(y, Cb, Cr, c.a);
}

subroutine(FilterFunc)
vec4 rgbsplit_filter(vec2 uv) {
  float x_offset = 10.0; // px
  float y_offset = 10.0; // px
  vec2 delta = vec2(x_offset / screen_size.x, y_offset / screen_size.y);
  vec4 c1 = texture(image, uv + delta);
  vec4 c2 = texture(image, uv);
  vec4 c3 = texture(image, uv - delta);
  return vec4(c1.r, c2.g, c3.b, c1.a + c2.a + c3.b);
}

subroutine(FilterFunc)
vec4 barrel_distort_filter(vec2 uv) {
  vec2 cc = uv - 0.5;
  uv = uv + (cc * dot(cc, cc) * amount.x);
  return texture(image, clamp(uv, 0.0, 1.0));
}

subroutine(FilterFunc)
vec4 gaus_filter_1(vec2 uv) {
  float dy = 1.0 / screen_size.y;
  vec4 sum = texture(image, uv) * weights[0];

  for (int i = 1; i < 5; i++) {
    vec2 offset_ = vec2(0.0, float(i)) * dy;
    sum += texture(image, clamp(uv + offset_, 0.0, 1.0)) * weights[i];
    sum += texture(image, clamp(uv - offset_, 0.0, 1.0)) * weights[i];
  }

  return sum;
}

subroutine(FilterFunc)
vec4 gaus_filter_2(vec2 uv) {
  float dx = 1.0 / screen_size.x;
  vec4 sum = texture(image, uv) * weights[0];

  for (int i = 1; i < 5; i++) {
    vec2 offset_ = vec2(float(i), 0.0) * dx;
    sum += texture(image, clamp(uv + offset_, 0.0, 1.0)) * weights[i];
    sum += texture(image, clamp(uv - offset_, 0.0, 1.0)) * weights[i];
  }

  return sum;
}

subroutine(FilterFunc)
vec4 horizontal_clamp_filter(vec2 uv) {
  float x = max(uv.x, amount.x);
  x = min(x, amount.y);
  return texture(image, vec2(x, uv.y));
}

subroutine(FilterFunc)
vec4 vertical_clamp_filter(vec2 uv) {
  float y = max(uv.y, amount.x);
  y = min(y, amount.y);
  return texture(image, vec2(uv.x, y));
}

void main () {
  color = FilterEffect(uv);
}
