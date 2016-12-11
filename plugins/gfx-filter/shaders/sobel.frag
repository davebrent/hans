#version 150

in vec2 uv;
out vec4 color;

uniform sampler2D u_texture;
uniform vec2 u_resolution;

void main(void) {
	float x = 1.0 / u_resolution.x;
	float y = 1.0 / u_resolution.y;

	vec4 horiz_edge = vec4(0.0);
	horiz_edge -= texture(u_texture, vec2(uv.x - x, uv.y - y)) * 1.0;
	horiz_edge -= texture(u_texture, vec2(uv.x - x, uv.y    )) * 2.0;
	horiz_edge -= texture(u_texture, vec2(uv.x - x, uv.y + y)) * 1.0;
	horiz_edge += texture(u_texture, vec2(uv.x + x, uv.y - y)) * 1.0;
	horiz_edge += texture(u_texture, vec2(uv.x + x, uv.y    )) * 2.0;
	horiz_edge += texture(u_texture, vec2(uv.x + x, uv.y + y)) * 1.0;

	vec4 vert_edge = vec4(0.0);
	vert_edge -= texture(u_texture, vec2(uv.x - x, uv.y - y)) * 1.0;
	vert_edge -= texture(u_texture, vec2(uv.x    , uv.y - y)) * 2.0;
	vert_edge -= texture(u_texture, vec2(uv.x + x, uv.y - y)) * 1.0;
	vert_edge += texture(u_texture, vec2(uv.x - x, uv.y + y)) * 1.0;
	vert_edge += texture(u_texture, vec2(uv.x    , uv.y + y)) * 2.0;
	vert_edge += texture(u_texture, vec2(uv.x + x, uv.y + y)) * 1.0;

	vec3 edge = sqrt((horiz_edge.rgb * horiz_edge.rgb) +
                   (vert_edge.rgb * vert_edge.rgb));

	color = vec4(edge, texture(u_texture, uv).a);
}

