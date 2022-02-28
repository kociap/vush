#include <anton/flat_hash_map.hpp>
#include <anton/string_view.hpp>
#include <builtins.hpp>
#include <memory.hpp>
#include <parser.hpp>

namespace vush {
    using namespace anton::literals;

    constexpr anton::String_View builtin_functions_declarations_source = R"(float radians(float degrees) {}
vec2 radians(vec2 degrees) {}
vec3 radians(vec3 degrees) {}
vec4 radians(vec4 degrees) {}
float degrees(float radians) {}
vec2 degrees(vec2 radians) {}
vec3 degrees(vec3 radians) {}
vec4 degrees(vec4 radians) {}
float sin(float angle) {}
vec2 sin(vec2 angle) {}
vec3 sin(vec3 angle) {}
vec4 sin(vec4 angle) {}
float cos(float angle) {}
vec2 cos(vec2 angle) {}
vec3 cos(vec3 angle) {}
vec4 cos(vec4 angle) {}
float tan(float angle) {}
vec2 tan(vec2 angle) {}
vec3 tan(vec3 angle) {}
vec4 tan(vec4 angle) {}
float asin(float x) {}
vec2 asin(vec2 x) {}
vec3 asin(vec3 x) {}
vec4 asin(vec4 x) {}
float acos(float x) {}
vec2 acos(vec2 x) {}
vec3 acos(vec3 x) {}
vec4 acos(vec4 x) {}
float atan(float y, float x) {}
vec2 atan(vec2 y, vec2 x) {}
vec3 atan(vec3 y, vec3 x) {}
vec4 atan(vec4 y, vec4 x) {}
float atan(float y_over_x) {}
vec2 atan(vec2 y_over_x) {}
vec3 atan(vec3 y_over_x) {}
vec4 atan(vec4 y_over_x) {}
float sinh(float x) {}
vec2 sinh(vec2 x) {}
vec3 sinh(vec3 x) {}
vec4 sinh(vec4 x) {}
float cosh(float x) {}
vec2 cosh(vec2 x) {}
vec3 cosh(vec3 x) {}
vec4 cosh(vec4 x) {}
float tanh(float x) {}
vec2 tanh(vec2 x) {}
vec3 tanh(vec3 x) {}
vec4 tanh(vec4 x) {}
float asinh(float x) {}
vec2 asinh(vec2 x) {}
vec3 asinh(vec3 x) {}
vec4 asinh(vec4 x) {}
float acosh(float x) {}
vec2 acosh(vec2 x) {}
vec3 acosh(vec3 x) {}
vec4 acosh(vec4 x) {}
float atanh(float x) {}
vec2 atanh(vec2 x) {}
vec3 atanh(vec3 x) {}
vec4 atanh(vec4 x) {}
float pow(float x) {}
vec2 pow(vec2 x) {}
vec3 pow(vec3 x) {}
vec4 pow(vec4 x) {}
float exp(float x) {}
vec2 exp(vec2 x) {}
vec3 exp(vec3 x) {}
vec4 exp(vec4 x) {}
float log(float x) {}
vec2 log(vec2 x) {}
vec3 log(vec3 x) {}
vec4 log(vec4 x) {}
float exp2(float x) {}
vec2 exp2(vec2 x) {}
vec3 exp2(vec3 x) {}
vec4 exp2(vec4 x) {}
float log2(float x) {}
vec2 log2(vec2 x) {}
vec3 log2(vec3 x) {}
vec4 log2(vec4 x) {}
float sqrt(float x) {}
vec2 sqrt(vec2 x) {}
vec3 sqrt(vec3 x) {}
vec4 sqrt(vec4 x) {}
double sqrt(double x) {}
dvec2 sqrt(dvec2 x) {}
dvec3 sqrt(dvec3 x) {}
dvec4 sqrt(dvec4 x) {}
float inversesqrt(float x) {}
vec2 inversesqrt(vec2 x) {}
vec3 inversesqrt(vec3 x) {}
vec4 inversesqrt(vec4 x) {}
double inversesqrt(double x) {}
dvec2 inversesqrt(dvec2 x) {}
dvec3 inversesqrt(dvec3 x) {}
dvec4 inversesqrt(dvec4 x) {}
float abs(float x) {}
vec2 abs(vec2 x) {}
vec3 abs(vec3 x) {}
vec4 abs(vec4 x) {}
double abs(double x) {}
dvec2 abs(dvec2 x) {}
dvec3 abs(dvec3 x) {}
dvec4 abs(dvec4 x) {}
int abs(int x) {}
ivec2 abs(ivec2 x) {}
ivec3 abs(ivec3 x) {}
ivec4 abs(ivec4 x) {}
float sign(float x) {}
vec2 sign(vec2 x) {}
vec3 sign(vec3 x) {}
vec4 sign(vec4 x) {}
double sign(double x) {}
dvec2 sign(dvec2 x) {}
dvec3 sign(dvec3 x) {}
dvec4 sign(dvec4 x) {}
int sign(int x) {}
ivec2 sign(ivec2 x) {}
ivec3 sign(ivec3 x) {}
ivec4 sign(ivec4 x) {}
float floor(float x) {}
vec2 floor(vec2 x) {}
vec3 floor(vec3 x) {}
vec4 floor(vec4 x) {}
double floor(double x) {}
dvec2 floor(dvec2 x) {}
dvec3 floor(dvec3 x) {}
dvec4 floor(dvec4 x) {}
float trunc(float x) {}
vec2 trunc(vec2 x) {}
vec3 trunc(vec3 x) {}
vec4 trunc(vec4 x) {}
double trunc(double x) {}
dvec2 trunc(dvec2 x) {}
dvec3 trunc(dvec3 x) {}
dvec4 trunc(dvec4 x) {}
float round(float x) {}
vec2 round(vec2 x) {}
vec3 round(vec3 x) {}
vec4 round(vec4 x) {}
double round(double x) {}
dvec2 round(dvec2 x) {}
dvec3 round(dvec3 x) {}
dvec4 round(dvec4 x) {}
float roundEven(float x) {}
vec2 roundEven(vec2 x) {}
vec3 roundEven(vec3 x) {}
vec4 roundEven(vec4 x) {}
double roundEven(double x) {}
dvec2 roundEven(dvec2 x) {}
dvec3 roundEven(dvec3 x) {}
dvec4 roundEven(dvec4 x) {}
float ceil(float x) {}
vec2 ceil(vec2 x) {}
vec3 ceil(vec3 x) {}
vec4 ceil(vec4 x) {}
double ceil(double x) {}
dvec2 ceil(dvec2 x) {}
dvec3 ceil(dvec3 x) {}
dvec4 ceil(dvec4 x) {}
float fract(float x) {}
vec2 fract(vec2 x) {}
vec3 fract(vec3 x) {}
vec4 fract(vec4 x) {}
double fract(double x) {}
dvec2 fract(dvec2 x) {}
dvec3 fract(dvec3 x) {}
dvec4 fract(dvec4 x) {}
float mod(float x, float y) {}
vec2 mod(vec2 x, float y) {}
vec3 mod(vec3 x, float y) {}
vec4 mod(vec4 x, float y) {}
double mod(double x, double y) {}
dvec2 mod(dvec2 x, double y) {}
dvec3 mod(dvec3 x, double y) {}
dvec4 mod(dvec4 x, double y) {}
float mod(float x, float y) {}
vec2 mod(vec2 x, vec2 y) {}
vec3 mod(vec3 x, vec3 y) {}
vec4 mod(vec4 x, vec4 y) {}
double mod(double x, double y) {}
dvec2 mod(dvec2 x, dvec2 y) {}
dvec3 mod(dvec3 x, dvec3 y) {}
dvec4 mod(dvec4 x, dvec4 y) {}
float modf(float x, float y) {}
vec2 modf(vec2 x, vec2 y) {}
vec3 modf(vec3 x, vec3 y) {}
vec4 modf(vec4 x, vec4 y) {}
double modf(double x, double y) {}
dvec2 modf(dvec2 x, dvec2 y) {}
dvec3 modf(dvec3 x, dvec3 y) {}
dvec4 modf(dvec4 x, dvec4 y) {}
float min(float x, float y) {}
vec2 min(vec2 x, float y) {}
vec3 min(vec3 x, float y) {}
vec4 min(vec4 x, float y) {}
double min(double x, double y) {}
dvec2 min(dvec2 x, double y) {}
dvec3 min(dvec3 x, double y) {}
dvec4 min(dvec4 x, double y) {}
int min(int x, int y) {}
ivec2 min(ivec2 x, int y) {}
ivec3 min(ivec3 x, int y) {}
ivec4 min(ivec4 x, int y) {}
uint min(uint x, uint y) {}
uvec2 min(uvec2 x, uint y) {}
uvec3 min(uvec3 x, uint y) {}
uvec4 min(uvec4 x, uint y) {}
float min(float x, float y) {}
vec2 min(vec2 x, vec2 y) {}
vec3 min(vec3 x, vec3 y) {}
vec4 min(vec4 x, vec4 y) {}
double min(double x, double y) {}
dvec2 min(dvec2 x, dvec2 y) {}
dvec3 min(dvec3 x, dvec3 y) {}
dvec4 min(dvec4 x, dvec4 y) {}
int min(int x, int y) {}
ivec2 min(ivec2 x, ivec2 y) {}
ivec3 min(ivec3 x, ivec3 y) {}
ivec4 min(ivec4 x, ivec4 y) {}
uint min(uint x, uint y) {}
uvec2 min(uvec2 x, uvec2 y) {}
uvec3 min(uvec3 x, uvec3 y) {}
uvec4 min(uvec4 x, uvec4 y) {}
float max(float x, float y) {}
vec2 max(vec2 x, float y) {}
vec3 max(vec3 x, float y) {}
vec4 max(vec4 x, float y) {}
double max(double x, double y) {}
dvec2 max(dvec2 x, double y) {}
dvec3 max(dvec3 x, double y) {}
dvec4 max(dvec4 x, double y) {}
int max(int x, int y) {}
ivec2 max(ivec2 x, int y) {}
ivec3 max(ivec3 x, int y) {}
ivec4 max(ivec4 x, int y) {}
uint max(uint x, uint y) {}
uvec2 max(uvec2 x, uint y) {}
uvec3 max(uvec3 x, uint y) {}
uvec4 max(uvec4 x, uint y) {}
float max(float x, float y) {}
vec2 max(vec2 x, vec2 y) {}
vec3 max(vec3 x, vec3 y) {}
vec4 max(vec4 x, vec4 y) {}
double max(double x, double y) {}
dvec2 max(dvec2 x, dvec2 y) {}
dvec3 max(dvec3 x, dvec3 y) {}
dvec4 max(dvec4 x, dvec4 y) {}
int max(int x, int y) {}
ivec2 max(ivec2 x, ivec2 y) {}
ivec3 max(ivec3 x, ivec3 y) {}
ivec4 max(ivec4 x, ivec4 y) {}
uint max(uint x, uint y) {}
uvec2 max(uvec2 x, uvec2 y) {}
uvec3 max(uvec3 x, uvec3 y) {}
uvec4 max(uvec4 x, uvec4 y) {}
float clamp(float x, float min_val, float max_val) {}
vec2 clamp(vec2 x, float min_val, float max_val) {}
vec3 clamp(vec3 x, float min_val, float max_val) {}
vec4 clamp(vec4 x, float min_val, float max_val) {}
double clamp(double x, double min_val, double max_val) {}
dvec2 clamp(dvec2 x, double min_val, double max_val) {}
dvec3 clamp(dvec3 x, double min_val, double max_val) {}
dvec4 clamp(dvec4 x, double min_val, double max_val) {}
int clamp(int x, int min_val, int max_val) {}
ivec2 clamp(ivec2 x, int min_val, int max_val) {}
ivec3 clamp(ivec3 x, int min_val, int max_val) {}
ivec4 clamp(ivec4 x, int min_val, int max_val) {}
uint clamp(uint x, uint min_val, uint max_val) {}
uvec2 clamp(uvec2 x, uint min_val, uint max_val) {}
uvec3 clamp(uvec3 x, uint min_val, uint max_val) {}
uvec4 clamp(uvec4 x, uint min_val, uint max_val) {}
float clamp(float x, float min_val, float max_val) {}
vec2 clamp(vec2 x, vec2 min_val, vec2 max_val) {}
vec3 clamp(vec3 x, vec3 min_val, vec3 max_val) {}
vec4 clamp(vec4 x, vec4 min_val, vec4 max_val) {}
double clamp(double x, double min_val, double max_val) {}
dvec2 clamp(dvec2 x, dvec2 min_val, dvec2 max_val) {}
dvec3 clamp(dvec3 x, dvec3 min_val, dvec3 max_val) {}
dvec4 clamp(dvec4 x, dvec4 min_val, dvec4 max_val) {}
int clamp(int x, int min_val, int max_val) {}
ivec2 clamp(ivec2 x, ivec2 min_val, ivec2 max_val) {}
ivec3 clamp(ivec3 x, ivec3 min_val, ivec3 max_val) {}
ivec4 clamp(ivec4 x, ivec4 min_val, ivec4 max_val) {}
uint clamp(uint x, uint min_val, uint max_val) {}
uvec2 clamp(uvec2 x, uvec2 min_val, uvec2 max_val) {}
uvec3 clamp(uvec3 x, uvec3 min_val, uvec3 max_val) {}
uvec4 clamp(uvec4 x, uvec4 min_val, uvec4 max_val) {}
float mix(float x, float y, float a) {}
vec2 mix(vec2 x, vec2 y, float a) {}
vec3 mix(vec3 x, vec3 y, float a) {}
vec4 mix(vec4 x, vec4 y, float a) {}
double mix(double x, double y, double a) {}
dvec2 mix(dvec2 x, dvec2 y, double a) {}
dvec3 mix(dvec3 x, dvec3 y, double a) {}
dvec4 mix(dvec4 x, dvec4 y, double a) {}
float mix(float x, float y, float a) {}
vec2 mix(vec2 x, vec2 y, vec2 a) {}
vec3 mix(vec3 x, vec3 y, vec3 a) {}
vec4 mix(vec4 x, vec4 y, vec4 a) {}
double mix(double x, double y, double a) {}
dvec2 mix(dvec2 x, dvec2 y, dvec2 a) {}
dvec3 mix(dvec3 x, dvec3 y, dvec3 a) {}
dvec4 mix(dvec4 x, dvec4 y, dvec4 a) {}
float mix(float x, float y, bool a) {}
vec2 mix(vec2 x, vec2 y, bvec2 a) {}
vec3 mix(vec3 x, vec3 y, bvec3 a) {}
vec4 mix(vec4 x, vec4 y, bvec4 a) {}
double mix(double x, double y, bool a) {}
dvec2 mix(dvec2 x, dvec2 y, bvec2 a) {}
dvec3 mix(dvec3 x, dvec3 y, bvec3 a) {}
dvec4 mix(dvec4 x, dvec4 y, bvec4 a) {}
int mix(int x, int y, bool a) {}
ivec2 mix(ivec2 x, ivec2 y, bvec2 a) {}
ivec3 mix(ivec3 x, ivec3 y, bvec3 a) {}
ivec4 mix(ivec4 x, ivec4 y, bvec4 a) {}
uint mix(uint x, uint y, bool a) {}
uvec2 mix(uvec2 x, uvec2 y, bvec2 a) {}
uvec3 mix(uvec3 x, uvec3 y, bvec3 a) {}
uvec4 mix(uvec4 x, uvec4 y, bvec4 a) {}
bool mix(bool x, bool y, bool a) {}
bvec2 mix(bvec2 x, bvec2 y, bvec2 a) {}
bvec3 mix(bvec3 x, bvec3 y, bvec3 a) {}
bvec4 mix(bvec4 x, bvec4 y, bvec4 a) {}
float step(float edge, float x) {}
vec2 step(float edge, vec2 x) {}
vec3 step(float edge, vec3 x) {}
vec4 step(float edge, vec4 x) {}
double step(double edge, double x) {}
dvec2 step(double edge, dvec2 x) {}
dvec3 step(double edge, dvec3 x) {}
dvec4 step(double edge, dvec4 x) {}
float step(float edge, float x) {}
vec2 step(vec2 edge, vec2 x) {}
vec3 step(vec3 edge, vec3 x) {}
vec4 step(vec4 edge, vec4 x) {}
double step(double edge, double x) {}
dvec2 step(dvec2 edge, dvec2 x) {}
dvec3 step(dvec3 edge, dvec3 x) {}
dvec4 step(dvec4 edge, dvec4 x) {}
float smoothstep(float edge0, float edge1, float x) {}
vec2 smoothstep(vec2 edge0, vec2 edge1, vec2 x) {}
vec3 smoothstep(vec3 edge0, vec3 edge1, vec3 x) {}
vec4 smoothstep(vec4 edge0, vec4 edge1, vec4 x) {}
double smoothstep(double edge0, double edge1, double x) {}
dvec2 smoothstep(dvec2 edge0, dvec2 edge1, dvec2 x) {}
dvec3 smoothstep(dvec3 edge0, dvec3 edge1, dvec3 x) {}
dvec4 smoothstep(dvec4 edge0, dvec4 edge1, dvec4 x) {}
float smoothstep(float edge0, float edge1, float x) {}
vec2 smoothstep(float edge0, float edge1, vec2 x) {}
vec3 smoothstep(float edge0, float edge1, vec3 x) {}
vec4 smoothstep(float edge0, float edge1, vec4 x) {}
double smoothstep(double edge0, double edge1, double x) {}
dvec2 smoothstep(double edge0, double edge1, dvec2 x) {}
dvec3 smoothstep(double edge0, double edge1, dvec3 x) {}
dvec4 smoothstep(double edge0, double edge1, dvec4 x) {}
bool isnan(float x) {}
bvec2 isnan(vec2 x) {}
bvec3 isnan(vec3 x) {}
bvec4 isnan(vec4 x) {}
bool isnan(double x) {}
bvec2 isnan(dvec2 x) {}
bvec3 isnan(dvec3 x) {}
bvec4 isnan(dvec4 x) {}
bool isinf(float x) {}
bvec2 isinf(vec2 x) {}
bvec3 isinf(vec3 x) {}
bvec4 isinf(vec4 x) {}
bool isinf(double x) {}
bvec2 isinf(dvec2 x) {}
bvec3 isinf(dvec3 x) {}
bvec4 isinf(dvec4 x) {}
int floatBitsToint(float x) {}
ivec2 floatBitsToint(vec2 x) {}
ivec3 floatBitsToint(vec3 x) {}
ivec4 floatBitsToint(vec4 x) {}
uint floatBitsToUint(float x) {}
uvec2 floatBitsToUint(vec2 x) {}
uvec3 floatBitsToUint(vec3 x) {}
uvec4 floatBitsToUint(vec4 x) {}
float intBitsToFloat(int x) {}
vec2 intBitsToFloat(ivec2 x) {}
vec3 intBitsToFloat(ivec3 x) {}
vec4 intBitsToFloat(ivec4 x) {}
float uintBitsToFloat(uint x) {}
vec2 uintBitsToFloat(uvec2 x) {}
vec3 uintBitsToFloat(uvec3 x) {}
vec4 uintBitsToFloat(uvec4 x) {}
float fma(float a, float b, float c) {}
vec2 fma(vec2 a, vec2 b, vec2 c) {}
vec3 fma(vec3 a, vec3 b, vec3 c) {}
vec4 fma(vec4 a, vec4 b, vec4 c) {}
double fma(double a, double b, double c) {}
dvec2 fma(dvec2 a, dvec2 b, dvec2 c) {}
dvec3 fma(dvec3 a, dvec3 b, dvec3 c) {}
dvec4 fma(dvec4 a, dvec4 b, dvec4 c) {}
float frexp(float x, float exp) {}
vec2 frexp(vec2 x, vec2 exp) {}
vec3 frexp(vec3 x, vec3 exp) {}
vec4 frexp(vec4 x, vec4 exp) {}
double frexp(double x, double exp) {}
dvec2 frexp(dvec2 x, dvec2 exp) {}
dvec3 frexp(dvec3 x, dvec3 exp) {}
dvec4 frexp(dvec4 x, dvec4 exp) {}
float ldexp(float x, float exp) {}
vec2 ldexp(vec2 x, vec2 exp) {}
vec3 ldexp(vec3 x, vec3 exp) {}
vec4 ldexp(vec4 x, vec4 exp) {}
double ldexp(double x, double exp) {}
dvec2 ldexp(dvec2 x, dvec2 exp) {}
dvec3 ldexp(dvec3 x, dvec3 exp) {}
dvec4 ldexp(dvec4 x, dvec4 exp) {}
uint packUnorm2x16(vec2 v) {}
uint packSnorm2x16(vec2 v) {}
uint packUnorm4x8(vec4 v) {}
uint packSnorm4x8(vec4 v) {}
vec2 unpackUnorm2x16(uint p) {}
vec2 unpackSnorm2x16(uint p) {}
vec4 unpackUnorm4x8(uint p) {}
vec4 unpackSnorm4x8(uint p) {}
uint packHalf2x16(vec2 v) {}
vec2 unpackHalf2x16(uint v) {}
double packDouble2x32(uvec2 v) {}
uvec2 unpackDouble2x32(double v) {}
float length(float x) {}
float length(vec2 x) {}
float length(vec3 x) {}
float length(vec4 x) {}
double length(double x) {}
double length(dvec2 x) {}
double length(dvec3 x) {}
double length(dvec4 x) {}
float distance(float p0, float p1) {}
float distance(vec2 p0, vec2 p1) {}
float distance(vec3 p0, vec3 p1) {}
float distance(vec4 p0, vec4 p1) {}
double distance(double p0, double p1) {}
double distance(dvec2 p0, dvec2 p1) {}
double distance(dvec3 p0, dvec3 p1) {}
double distance(dvec4 p0, dvec4 p1) {}
float distance(float x, float y) {}
float distance(vec2 x, vec2 y) {}
float distance(vec3 x, vec3 y) {}
float distance(vec4 x, vec4 y) {}
double distance(double x, double y) {}
double distance(dvec2 x, dvec2 y) {}
double distance(dvec3 x, dvec3 y) {}
double distance(dvec4 x, dvec4 y) {}
float dot(float x, float y) {}
float dot(vec2 x, vec2 y) {}
float dot(vec3 x, vec3 y) {}
float dot(vec4 x, vec4 y) {}
double dot(double x, double y) {}
double dot(dvec2 x, dvec2 y) {}
double dot(dvec3 x, dvec3 y) {}
double dot(dvec4 x, dvec4 y) {}
vec3 cross(vec3 x, vec3 y) {}
dvec3 cross(dvec3 x, dvec3 y) {}
float normalize(float x) {}
vec2 normalize(vec2 x) {}
vec3 normalize(vec3 x) {}
vec4 normalize(vec4 x) {}
double normalize(double x) {}
dvec2 normalize(dvec2 x) {}
dvec3 normalize(dvec3 x) {}
dvec4 normalize(dvec4 x) {}
float faceforward(float N, float I, float Nref) {}
vec2 faceforward(vec2 N, vec2 I, vec2 Nref) {}
vec3 faceforward(vec3 N, vec3 I, vec3 Nref) {}
vec4 faceforward(vec4 N, vec4 I, vec4 Nref) {}
double faceforward(double N, double I, double Nref) {}
dvec2 faceforward(dvec2 N, dvec2 I, dvec2 Nref) {}
dvec3 faceforward(dvec3 N, dvec3 I, dvec3 Nref) {}
dvec4 faceforward(dvec4 N, dvec4 I, dvec4 Nref) {}
float reflect(float I, float N) {}
vec2 reflect(vec2 I, vec2 N) {}
vec3 reflect(vec3 I, vec3 N) {}
vec4 reflect(vec4 I, vec4 N) {}
double reflect(double I, double N) {}
dvec2 reflect(dvec2 I, dvec2 N) {}
dvec3 reflect(dvec3 I, dvec3 N) {}
dvec4 reflect(dvec4 I, dvec4 N) {}
float refract(float I, float N, float eta) {}
vec2 refract(vec2 I, vec2 N, float eta) {}
vec3 refract(vec3 I, vec3 N, float eta) {}
vec4 refract(vec4 I, vec4 N, float eta) {}
double refract(double I, double N, double eta) {}
dvec2 refract(dvec2 I, dvec2 N, double eta) {}
dvec3 refract(dvec3 I, dvec3 N, double eta) {}
dvec4 refract(dvec4 I, dvec4 N, double eta) {}
mat2 matrixCompMult(mat2 x, mat2 y) {}
mat3 matrixCompMult(mat3 x, mat3 y) {}
mat4 matrixCompMult(mat4 x, mat4 y) {}
mat2x3 matrixCompMult(mat2x3 x, mat2x3 y) {}
mat2x4 matrixCompMult(mat2x4 x, mat2x4 y) {}
mat3x2 matrixCompMult(mat3x2 x, mat3x2 y) {}
mat3x4 matrixCompMult(mat3x4 x, mat3x4 y) {}
mat4x2 matrixCompMult(mat4x2 x, mat4x2 y) {}
mat4x3 matrixCompMult(mat4x3 x, mat4x3 y) {}
mat2 outerProduct(vec2 c, vec2 r) {}
mat3 outerProduct(vec3 c, vec3 r) {}
mat4 outerProduct(vec4 c, vec4 r) {}
mat2x3 outerProduct(vec3 c, vec2 r) {}
mat3x2 outerProduct(vec2 c, vec3 r) {}
mat2x4 outerProduct(vec4 c, vec2 r) {}
mat4x2 outerProduct(vec2 c, vec4 r) {}
mat3x4 outerProduct(vec4 c, vec3 r) {}
mat4x3 outerProduct(vec3 c, vec4 r) {}
mat2 transpose(mat2 m) {}
mat3 transpose(mat3 m) {}
mat4 transpose(mat4 m) {}
mat2x3 transpose(mat3x2 m) {}
mat3x2 transpose(mat2x3 m) {}
mat2x4 transpose(mat4x2 m) {}
mat4x2 transpose(mat2x4 m) {}
mat3x4 transpose(mat4x3 m) {}
mat4x3 transpose(mat3x4 m) {}
float determinant(mat2 m) {}
float determinant(mat3 m) {}
float determinant(mat4 m) {}
mat2 inverse(mat2 m) {}
mat3 inverse(mat3 m) {}
mat4 inverse(mat4 m) {}
bvec2 lessThan(vec2 x, vec2 y) {}
bvec3 lessThan(vec3 x, vec3 y) {}
bvec4 lessThan(vec4 x, vec4 y) {}
bvec2 lessThan(dvec2 x, dvec2 y) {}
bvec3 lessThan(dvec3 x, dvec3 y) {}
bvec4 lessThan(dvec4 x, dvec4 y) {}
bvec2 lessThan(uvec2 x, uvec2 y) {}
bvec3 lessThan(uvec3 x, uvec3 y) {}
bvec4 lessThan(uvec4 x, uvec4 y) {}
bvec2 lessThan(ivec2 x, ivec2 y) {}
bvec3 lessThan(ivec3 x, ivec3 y) {}
bvec4 lessThan(ivec4 x, ivec4 y) {}
bvec2 lessThanEqual(vec2 x, vec2 y) {}
bvec3 lessThanEqual(vec3 x, vec3 y) {}
bvec4 lessThanEqual(vec4 x, vec4 y) {}
bvec2 lessThanEqual(dvec2 x, dvec2 y) {}
bvec3 lessThanEqual(dvec3 x, dvec3 y) {}
bvec4 lessThanEqual(dvec4 x, dvec4 y) {}
bvec2 lessThanEqual(uvec2 x, uvec2 y) {}
bvec3 lessThanEqual(uvec3 x, uvec3 y) {}
bvec4 lessThanEqual(uvec4 x, uvec4 y) {}
bvec2 lessThanEqual(ivec2 x, ivec2 y) {}
bvec3 lessThanEqual(ivec3 x, ivec3 y) {}
bvec4 lessThanEqual(ivec4 x, ivec4 y) {}
bvec2 greaterThan(vec2 x, vec2 y) {}
bvec3 greaterThan(vec3 x, vec3 y) {}
bvec4 greaterThan(vec4 x, vec4 y) {}
bvec2 greaterThan(dvec2 x, dvec2 y) {}
bvec3 greaterThan(dvec3 x, dvec3 y) {}
bvec4 greaterThan(dvec4 x, dvec4 y) {}
bvec2 greaterThan(uvec2 x, uvec2 y) {}
bvec3 greaterThan(uvec3 x, uvec3 y) {}
bvec4 greaterThan(uvec4 x, uvec4 y) {}
bvec2 greaterThan(ivec2 x, ivec2 y) {}
bvec3 greaterThan(ivec3 x, ivec3 y) {}
bvec4 greaterThan(ivec4 x, ivec4 y) {}
bvec2 greaterThanEqual(vec2 x, vec2 y) {}
bvec3 greaterThanEqual(vec3 x, vec3 y) {}
bvec4 greaterThanEqual(vec4 x, vec4 y) {}
bvec2 greaterThanEqual(dvec2 x, dvec2 y) {}
bvec3 greaterThanEqual(dvec3 x, dvec3 y) {}
bvec4 greaterThanEqual(dvec4 x, dvec4 y) {}
bvec2 greaterThanEqual(uvec2 x, uvec2 y) {}
bvec3 greaterThanEqual(uvec3 x, uvec3 y) {}
bvec4 greaterThanEqual(uvec4 x, uvec4 y) {}
bvec2 greaterThanEqual(ivec2 x, ivec2 y) {}
bvec3 greaterThanEqual(ivec3 x, ivec3 y) {}
bvec4 greaterThanEqual(ivec4 x, ivec4 y) {}
bvec2 equal(vec2 x, vec2 y) {}
bvec3 equal(vec3 x, vec3 y) {}
bvec4 equal(vec4 x, vec4 y) {}
bvec2 equal(dvec2 x, dvec2 y) {}
bvec3 equal(dvec3 x, dvec3 y) {}
bvec4 equal(dvec4 x, dvec4 y) {}
bvec2 equal(uvec2 x, uvec2 y) {}
bvec3 equal(uvec3 x, uvec3 y) {}
bvec4 equal(uvec4 x, uvec4 y) {}
bvec2 equal(ivec2 x, ivec2 y) {}
bvec3 equal(ivec3 x, ivec3 y) {}
bvec4 equal(ivec4 x, ivec4 y) {}
bvec2 notEqual(vec2 x, vec2 y) {}
bvec3 notEqual(vec3 x, vec3 y) {}
bvec4 notEqual(vec4 x, vec4 y) {}
bvec2 notEqual(dvec2 x, dvec2 y) {}
bvec3 notEqual(dvec3 x, dvec3 y) {}
bvec4 notEqual(dvec4 x, dvec4 y) {}
bvec2 notEqual(uvec2 x, uvec2 y) {}
bvec3 notEqual(uvec3 x, uvec3 y) {}
bvec4 notEqual(uvec4 x, uvec4 y) {}
bvec2 notEqual(ivec2 x, ivec2 y) {}
bvec3 notEqual(ivec3 x, ivec3 y) {}
bvec4 notEqual(ivec4 x, ivec4 y) {}
bool any(bvec2 x) {}
bool any(bvec3 x) {}
bool any(bvec4 x) {}
bool all(bvec2 x) {}
bool all(bvec3 x) {}
bool all(bvec4 x) {}
bvec2 not(bvec2 x) {}
bvec3 not(bvec3 x) {}
bvec4 not(bvec4 x) {}
uint uaddCarry(uint x, uint y, uint carry) {}
uvec2 uaddCarry(uvec2 x, uvec2 y, uvec2 carry) {}
uvec3 uaddCarry(uvec3 x, uvec3 y, uvec3 carry) {}
uvec4 uaddCarry(uvec4 x, uvec4 y, uvec4 carry) {}
uint usubBorrow(uint x, uint y, uint borrow) {}
uvec2 usubBorrow(uvec2 x, uvec2 y, uvec2 borrow) {}
uvec3 usubBorrow(uvec3 x, uvec3 y, uvec3 borrow) {}
uvec4 usubBorrow(uvec4 x, uvec4 y, uvec4 borrow) {}
void umulExtended(uint x, uint y, uint msb, uint lsb) {}
void umulExtended(uvec2 x, uvec2 y, uvec2 msb, uvec2 lsb) {}
void umulExtended(uvec3 x, uvec3 y, uvec3 msb, uvec3 lsb) {}
void umulExtended(uvec4 x, uvec4 y, uvec4 msb, uvec4 lsb) {}
void imulExtended(int x, int y, int msb, int lsb) {}
void imulExtended(ivec2 x, ivec2 y, ivec2 msb, ivec2 lsb) {}
void imulExtended(ivec3 x, ivec3 y, ivec3 msb, ivec3 lsb) {}
void imulExtended(ivec4 x, ivec4 y, ivec4 msb, ivec4 lsb) {}
void bitfieldExtract(int value, int offset, int bits) {}
void bitfieldExtract(ivec2 value, int offset, int bits) {}
void bitfieldExtract(ivec3 value, int offset, int bits) {}
void bitfieldExtract(ivec4 value, int offset, int bits) {}
void bitfieldExtract(uint value, int offset, int bits) {}
void bitfieldExtract(uvec2 value, int offset, int bits) {}
void bitfieldExtract(uvec3 value, int offset, int bits) {}
void bitfieldExtract(uvec4 value, int offset, int bits) {}
void bitfieldInsert(int base, int insert, int offset, int bits) {}
void bitfieldInsert(ivec2 base, ivec2 insert, int offset, int bits) {}
void bitfieldInsert(ivec3 base, ivec3 insert, int offset, int bits) {}
void bitfieldInsert(ivec4 base, ivec4 insert, int offset, int bits) {}
void bitfieldInsert(uint base, uint insert, int offset, int bits) {}
void bitfieldInsert(uvec2 base, uvec2 insert, int offset, int bits) {}
void bitfieldInsert(uvec3 base, uvec3 insert, int offset, int bits) {}
void bitfieldInsert(uvec4 base, uvec4 insert, int offset, int bits) {}
void bitfieldReverse(int base) {}
void bitfieldReverse(ivec2 base) {}
void bitfieldReverse(ivec3 base) {}
void bitfieldReverse(ivec4 base) {}
void bitfieldReverse(uint base) {}
void bitfieldReverse(uvec2 base) {}
void bitfieldReverse(uvec3 base) {}
void bitfieldReverse(uvec4 base) {}
void bitCount(int base) {}
void bitCount(ivec2 base) {}
void bitCount(ivec3 base) {}
void bitCount(ivec4 base) {}
void bitCount(uint base) {}
void bitCount(uvec2 base) {}
void bitCount(uvec3 base) {}
void bitCount(uvec4 base) {}
void findLSB(int base) {}
void findLSB(ivec2 base) {}
void findLSB(ivec3 base) {}
void findLSB(ivec4 base) {}
void findLSB(uint base) {}
void findLSB(uvec2 base) {}
void findLSB(uvec3 base) {}
void findLSB(uvec4 base) {}
void findMSB(int base) {}
void findMSB(ivec2 base) {}
void findMSB(ivec3 base) {}
void findMSB(ivec4 base) {}
void findMSB(uint base) {}
void findMSB(uvec2 base) {}
void findMSB(uvec3 base) {}
void findMSB(uvec4 base) {}
int textureSize(sampler1D sampler, int lod) {}
int textureSize(isampler1D sampler, int lod) {}
int textureSize(usampler1D sampler, int lod) {}
ivec2 textureSize(sampler2D sampler, int lod) {}
ivec2 textureSize(isampler2D sampler, int lod) {}
ivec2 textureSize(usampler2D sampler, int lod) {}
ivec3 textureSize(sampler3D sampler, int lod) {}
ivec3 textureSize(isampler3D sampler, int lod) {}
ivec3 textureSize(usampler3D sampler, int lod) {}
ivec2 textureSize(samplerCube sampler, int lod) {}
ivec2 textureSize(isamplerCube sampler, int lod) {}
ivec2 textureSize(usamplerCube sampler, int lod) {}
int textureSize(sampler1DShadow sampler, int lod) {}
ivec2 textureSize(sampler2DShadow sampler, int lod) {}
ivec2 textureSize(samplerCubeShadow sampler, int lod)  {}
ivec3 textureSize(samplerCubeArray sampler, int lod) {}
ivec3 textureSize(isamplerCubeArray sampler, int lod) {}
ivec3 textureSize(usamplerCubeArray sampler, int lod) {}
ivec3 textureSize(samplerCubeArrayShadow sampler, int lod) {}
ivec2 textureSize(sampler2DRect sampler) {}
ivec2 textureSize(isampler2DRect sampler) {}
ivec2 textureSize(usampler2DRect sampler) {}
ivec2 textureSize(sampler2DRectShadow sampler) {}
ivec2 textureSize(sampler1DArray sampler, int lod) {}
ivec2 textureSize(isampler1DArray sampler, int lod) {}
ivec2 textureSize(usampler1DArray sampler, int lod) {}
ivec2 textureSize(sampler1DArrayShadow sampler, int lod) {}
ivec3 textureSize(sampler2DArray sampler, int lod) {}
ivec3 textureSize(isampler2DArray sampler, int lod) {}
ivec3 textureSize(usampler2DArray sampler, int lod) {}
ivec3 textureSize(sampler2DArrayShadow sampler, int lod) {}
int textureSize(samplerBuffer sampler) {}
int textureSize(isamplerBuffer sampler) {}
int textureSize(usamplerBuffer sampler) {}
ivec2 textureSize(sampler2DMS sampler) {}
ivec2 textureSize(isampler2DMS sampler) {}
ivec2 textureSize(usampler2DMS sampler) {}
ivec3 textureSize(sampler2DMSArray sampler) {}
ivec3 textureSize(isampler2DMSArray sampler) {}
ivec3 textureSize(usampler2DMSArray sampler) {}
vec2 textureQueryLod(sampler1D sampler, float P) {}
vec2 textureQueryLod(isampler1D sampler, float P) {}
vec2 textureQueryLod(usampler1D sampler, float P) {}
vec2 textureQueryLod(sampler2D sampler, vec2 P) {}
vec2 textureQueryLod(isampler2D sampler, vec2 P) {}
vec2 textureQueryLod(usampler2D sampler, vec2 P) {}
vec2 textureQueryLod(sampler3D sampler, vec3 P) {}
vec2 textureQueryLod(isampler3D sampler, vec3 P) {}
vec2 textureQueryLod(usampler3D sampler, vec3 P) {}
vec2 textureQueryLod(samplerCube sampler, vec3 P) {}
vec2 textureQueryLod(isamplerCube sampler, vec3 P) {}
vec2 textureQueryLod(usamplerCube sampler, vec3 P) {}
vec2 textureQueryLod(sampler1DArray sampler, float P) {}
vec2 textureQueryLod(isampler1DArray sampler, float P) {}
vec2 textureQueryLod(usampler1DArray sampler, float P) {}
vec2 textureQueryLod(sampler2DArray sampler, vec2 P) {}
vec2 textureQueryLod(isampler2DArray sampler, vec2 P) {}
vec2 textureQueryLod(usampler2DArray sampler, vec2 P) {}
vec2 textureQueryLod(samplerCubeArray sampler, vec3 P) {}
vec2 textureQueryLod(isamplerCubeArray sampler, vec3 P) {}
vec2 textureQueryLod(usamplerCubeArray sampler, vec3 P) {}
vec2 textureQueryLod(sampler1DShadow sampler, float P) {}
vec2 textureQueryLod(sampler2DShadow sampler, vec2 P) {}
vec2 textureQueryLod(samplerCubeShadow sampler, vec3 P) {}
vec2 textureQueryLod(sampler1DArrayShadow sampler, float P) {}
vec2 textureQueryLod(sampler2DArrayShadow sampler, vec2 P) {}
vec2 textureQueryLod(samplerCubeArrayShadow sampler, vec3 P) {}
int textureQueryLevels(sampler1D sampler) {}
int textureQueryLevels(isampler1D sampler) {}
int textureQueryLevels(usampler1D sampler) {}
int textureQueryLevels(sampler2D sampler) {}
int textureQueryLevels(isampler2D sampler) {}
int textureQueryLevels(usampler2D sampler) {}
int textureQueryLevels(sampler3D sampler) {}
int textureQueryLevels(isampler3D sampler) {}
int textureQueryLevels(usampler3D sampler) {}
int textureQueryLevels(samplerCube sampler) {}
int textureQueryLevels(isamplerCube sampler) {}
int textureQueryLevels(usamplerCube sampler) {}
int textureQueryLevels(sampler1DArray sampler) {}
int textureQueryLevels(isampler1DArray sampler) {}
int textureQueryLevels(usampler1DArray sampler) {}
int textureQueryLevels(sampler2DArray sampler) {}
int textureQueryLevels(isampler2DArray sampler) {}
int textureQueryLevels(usampler2DArray sampler) {}
int textureQueryLevels(samplerCubeArray sampler) {}
int textureQueryLevels(isamplerCubeArray sampler) {}
int textureQueryLevels(usamplerCubeArray sampler) {}
int textureQueryLevels(sampler1DShadow sampler) {}
int textureQueryLevels(sampler2DShadow sampler) {}
int textureQueryLevels(samplerCubeShadow sampler) {}
int textureQueryLevels(sampler1DArrayShadow sampler) {}
int textureQueryLevels(sampler2DArrayShadow sampler) {}
int textureQueryLevels(samplerCubeArrayShadow sampler) {}
int textureSamples(sampler2DMS sampler) {}
int textureSamples(isampler2DMS sampler) {}
int textureSamples(usampler2DMS sampler) {}
int textureSamples(sampler2DMSArray sampler) {}
int textureSamples(isampler2DMSArray sampler) {}
int textureSamples(usampler2DMSArray sampler) {}
vec4 texture(sampler1D sampler, float P) {}
ivec4 texture(isampler1D sampler, float P) {}
uvec4 texture(usampler1D sampler, float P) {}
vec4 texture(sampler1D sampler, float P, float bias) {}
ivec4 texture(isampler1D sampler, float P, float bias) {}
uvec4 texture(usampler1D sampler, float P, float bias) {}
vec4 texture(sampler2D sampler, vec2 P) {}
ivec4 texture(isampler2D sampler, vec2 P) {}
uvec4 texture(usampler2D sampler, vec2 P) {}
vec4 texture(sampler2D sampler, vec2 P, float bias) {}
ivec4 texture(isampler2D sampler, vec2 P, float bias) {}
uvec4 texture(usampler2D sampler, vec2 P, float bias) {}
vec4 texture(sampler3D sampler, vec3 P) {}
ivec4 texture(isampler3D sampler, vec3 P) {}
uvec4 texture(usampler3D sampler, vec3 P) {}
vec4 texture(sampler3D sampler, vec3 P, float bias) {}
ivec4 texture(isampler3D sampler, vec3 P, float bias) {}
uvec4 texture(usampler3D sampler, vec3 P, float bias) {}
vec4 texture(samplerCube sampler, vec3 P) {}
ivec4 texture(isamplerCube sampler, vec3 P) {}
uvec4 texture(usamplerCube sampler, vec3 P) {}
vec4 texture(samplerCube sampler, vec3 P, float bias) {}
ivec4 texture(isamplerCube sampler, vec3 P, float bias) {}
uvec4 texture(usamplerCube sampler, vec3 P, float bias) {}
float texture(sampler1DShadow sampler, vec3 P) {}
float texture(sampler1DShadow sampler, vec3 P, float bias) {}
float texture(sampler2DShadow sampler, vec3 P) {}
float texture(sampler2DShadow sampler, vec3 P, float bias) {}
float texture(samplerCubeShadow sampler, vec4 P) {}
float texture(samplerCubeShadow sampler, vec4 P, float bias) {}
vec4 texture(sampler2DArray sampler, vec3 P) {}
ivec4 texture(isampler2DArray sampler, vec3 P) {}
uvec4 texture(usampler2DArray sampler, vec3 P) {}
vec4 texture(sampler2DArray sampler, vec3 P, float bias) {}
ivec4 texture(isampler2DArray sampler, vec3 P, float bias) {}
uvec4 texture(usampler2DArray sampler, vec3 P, float bias) {}
vec4 texture(samplerCubeArray sampler, vec4 P) {}
ivec4 texture(isamplerCubeArray sampler, vec4 P) {}
uvec4 texture(usamplerCubeArray sampler, vec4 P) {}
vec4 texture(samplerCubeArray sampler, vec4 P, float bias) {}
ivec4 texture(isamplerCubeArray sampler, vec4 P, float bias) {}
uvec4 texture(usamplerCubeArray sampler, vec4 P, float bias) {}
vec4 texture(sampler1DArray sampler, vec2 P) {}
ivec4 texture(isampler1DArray sampler, vec2 P) {}
uvec4 texture(usampler1DArray sampler, vec2 P) {}
vec4 texture(sampler1DArray sampler, vec2 P, float bias) {}
ivec4 texture(isampler1DArray sampler, vec2 P, float bias) {}
uvec4 texture(usampler1DArray sampler, vec2 P, float bias) {}
float texture(sampler1DArrayShadow sampler, vec3 P) {}
float texture(sampler1DArrayShadow sampler, vec3 P, float bias) {}
float texture(sampler2DArrayShadow sampler, vec4 P) {}
vec4 texture(sampler2DRect sampler, vec2 P) {}
ivec4 texture(isampler2DRect sampler, vec2 P) {}
uvec4 texture(usampler2DRect sampler, vec2 P) {}
float texture(sampler2DRectShadow sampler, vec3 P) {}
float texture(samplerCubeArrayShadow sampler, vec4 P, float compare) {}
vec4 textureProj(sampler1D sampler, vec2 P) {}
ivec4 textureProj(isampler1D sampler, vec2 P) {}
uvec4 textureProj(usampler1D sampler, vec2 P) {}
vec4 textureProj(sampler1D sampler, vec2 P, float bias) {}
ivec4 textureProj(isampler1D sampler, vec2 P, float bias) {}
uvec4 textureProj(usampler1D sampler, vec2 P, float bias) {}
vec4 textureProj(sampler1D sampler, vec4 P) {}
ivec4 textureProj(isampler1D sampler, vec4 P) {}
uvec4 textureProj(usampler1D sampler, vec4 P) {}
vec4 textureProj(sampler1D sampler, vec4 P, float bias) {}
ivec4 textureProj(isampler1D sampler, vec4 P, float bias) {}
uvec4 textureProj(usampler1D sampler, vec4 P, float bias) {}
vec4 textureProj(sampler2D sampler, vec3 P) {}
ivec4 textureProj(isampler2D sampler, vec3 P) {}
uvec4 textureProj(usampler2D sampler, vec3 P) {}
vec4 textureProj(sampler2D sampler, vec3 P, float bias) {}
ivec4 textureProj(isampler2D sampler, vec3 P, float bias) {}
uvec4 textureProj(usampler2D sampler, vec3 P, float bias) {}
vec4 textureProj(sampler2D sampler, vec4 P) {}
ivec4 textureProj(isampler2D sampler, vec4 P) {}
uvec4 textureProj(usampler2D sampler, vec4 P) {}
vec4 textureProj(sampler2D sampler, vec4 P, float bias) {}
ivec4 textureProj(isampler2D sampler, vec4 P, float bias) {}
uvec4 textureProj(usampler2D sampler, vec4 P, float bias) {}
vec4 textureProj(sampler3D sampler, vec4 P) {}
ivec4 textureProj(isampler3D sampler, vec4 P) {}
uvec4 textureProj(usampler3D sampler, vec4 P) {}
vec4 textureProj(sampler3D sampler, vec4 P, float bias) {}
ivec4 textureProj(isampler3D sampler, vec4 P, float bias) {}
uvec4 textureProj(usampler3D sampler, vec4 P, float bias) {}
float textureProj(sampler1DShadow sampler, vec4 P) {}
float textureProj(sampler1DShadow sampler, vec4 P, float bias) {}
float textureProj(sampler2DShadow sampler, vec4 P) {}
float textureProj(sampler2DShadow sampler, vec4 P, float bias) {}
vec4 textureProj(sampler2DRect sampler, vec3 P) {}
ivec4 textureProj(isampler2DRect sampler, vec3 P) {}
uvec4 textureProj(usampler2DRect sampler, vec3 P) {}
vec4 textureProj(sampler2DRect sampler, vec4 P) {}
ivec4 textureProj(isampler2DRect sampler, vec4 P) {}
uvec4 textureProj(usampler2DRect sampler, vec4 P) {}
float textureProj(sampler2DRectShadow sampler, vec4 P) {}
vec4 textureLod(sampler1D sampler, float P, float lod) {}
ivec4 textureLod(isampler1D sampler, float P, float lod) {}
uvec4 textureLod(usampler1D sampler, float P, float lod) {}
vec4 textureLod(sampler2D sampler, vec2 P, float lod) {}
ivec4 textureLod(isampler2D sampler, vec2 P, float lod) {}
uvec4 textureLod(usampler2D sampler, vec2 P, float lod) {}
vec4 textureLod(sampler3D sampler, vec3 P, float lod) {}
ivec4 textureLod(isampler3D sampler, vec3 P, float lod) {}
uvec4 textureLod(usampler3D sampler, vec3 P, float lod) {}
vec4 textureLod(samplerCube sampler, vec3 P, float lod) {}
ivec4 textureLod(isamplerCube sampler, vec3 P, float lod) {}
uvec4 textureLod(usamplerCube sampler, vec3 P, float lod) {}
float textureLod(sampler2DShadow sampler, vec3 P, float lod) {}
float textureLod(sampler1DShadow sampler, vec3 P, float lod) {}
vec4 textureLod(sampler1DArray sampler, vec2 P, float lod) {}
ivec4 textureLod(isampler1DArray sampler, vec2 P, float lod) {}
uvec4 textureLod(usampler1DArray sampler, vec2 P, float lod) {}
float textureLod(sampler1DArrayShadow sampler, vec3 P, float lod) {}
vec4 textureLod(sampler2DArray sampler, vec3 P, float lod) {}
ivec4 textureLod(isampler2DArray sampler, vec3 P, float lod) {}
uvec4 textureLod(usampler2DArray sampler, vec3 P, float lod) {}
vec4 textureLod(samplerCubeArray sampler, vec4 P, float lod) {}
ivec4 textureLod(isamplerCubeArray sampler, vec4 P, float lod) {}
uvec4 textureLod(usamplerCubeArray sampler, vec4 P, float lod) {}
vec4 textureOffset(sampler1D sampler, float P, int offset) {}
ivec4 textureOffset(isampler1D sampler, float P, int offset) {}
uvec4 textureOffset(usampler1D sampler, float P, int offset) {}
vec4 textureOffset(sampler1D sampler, float P, int offset, float bias) {}
ivec4 textureOffset(isampler1D sampler, float P, int offset, float bias) {}
uvec4 textureOffset(usampler1D sampler, float P, int offset, float bias) {}
vec4 textureOffset(sampler2D sampler, vec2 P, ivec2 offset) {}
ivec4 textureOffset(isampler2D sampler, vec2 P, ivec2 offset) {}
uvec4 textureOffset(usampler2D sampler, vec2 P, ivec2 offset) {}
vec4 textureOffset(sampler2D sampler, vec2 P, ivec2 offset, float bias) {}
ivec4 textureOffset(isampler2D sampler, vec2 P, ivec2 offset, float bias) {}
uvec4 textureOffset(usampler2D sampler, vec2 P, ivec2 offset, float bias) {}
vec4 textureOffset(sampler3D sampler, vec3 P, ivec3 offset) {}
ivec4 textureOffset(isampler3D sampler, vec3 P, ivec3 offset) {}
uvec4 textureOffset(usampler3D sampler, vec3 P, ivec3 offset) {}
vec4 textureOffset(sampler3D sampler, vec3 P, ivec3 offset, float bias) {}
ivec4 textureOffset(isampler3D sampler, vec3 P, ivec3 offset, float bias) {}
uvec4 textureOffset(usampler3D sampler, vec3 P, ivec3 offset, float bias) {}
float textureOffset(sampler2DShadow sampler, vec3 P, ivec2 offset) {}
float textureOffset(sampler2DShadow sampler, vec3 P, ivec2 offset, float bias) {}
vec4 textureOffset(sampler2DRect sampler, vec2 P, ivec2 offset) {}
ivec4 textureOffset(isampler2DRect sampler, vec2 P, ivec2 offset) {}
uvec4 textureOffset(usampler2DRect sampler, vec2 P, ivec2 offset) {}
float textureOffset(sampler2DRectShadow sampler, vec3 P, ivec2 offset) {}
float textureOffset(sampler1DShadow sampler, vec3 P, int offset) {}
float textureOffset(sampler1DShadow sampler, vec3 P, int offset, float bias) {}
vec4 textureOffset(sampler1DArray sampler, vec2 P, int offset) {}
ivec4 textureOffset(isampler1DArray sampler, vec2 P, int offset) {}
uvec4 textureOffset(usampler1DArray sampler, vec2 P, int offset) {}
vec4 textureOffset(sampler1DArray sampler, vec2 P, int offset, float bias) {}
ivec4 textureOffset(isampler1DArray sampler, vec2 P, int offset, float bias) {}
uvec4 textureOffset(usampler1DArray sampler, vec2 P, int offset, float bias) {}
vec4 textureOffset(sampler2DArray sampler, vec3 P, ivec2 offset) {}
ivec4 textureOffset(isampler2DArray sampler, vec3 P, ivec2 offset) {}
uvec4 textureOffset(usampler2DArray sampler, vec3 P, ivec2 offset) {}
vec4 textureOffset(sampler2DArray sampler, vec3 P, ivec2 offset, float bias) {}
ivec4 textureOffset(isampler2DArray sampler, vec3 P, ivec2 offset, float bias) {}
uvec4 textureOffset(usampler2DArray sampler, vec3 P, ivec2 offset, float bias) {}
float textureOffset(sampler1DArrayShadow sampler, vec3 P, int offset) {}
float textureOffset(sampler1DArrayShadow sampler, vec3 P, int offset, float bias) {}
float textureOffset(sampler2DArrayShadow sampler, vec4 P, ivec2 offset) {}
vec4 texelFetch(sampler1D sampler, int P, int lod) {}
ivec4 texelFetch(isampler1D sampler, int P, int lod) {}
uvec4 texelFetch(usampler1D sampler, int P, int lod) {}
vec4 texelFetch(sampler2D sampler, ivec2 P, int lod) {}
ivec4 texelFetch(isampler2D sampler, ivec2 P, int lod) {}
uvec4 texelFetch(usampler2D sampler, ivec2 P, int lod) {}
vec4 texelFetch(sampler3D sampler, ivec3 P, int lod)  {}
ivec4 texelFetch(isampler3D sampler, ivec3 P, int lod)  {}
uvec4 texelFetch(usampler3D sampler, ivec3 P, int lod)  {}
vec4 texelFetch(sampler2DRect sampler, ivec2 P) {}
ivec4 texelFetch(isampler2DRect sampler, ivec2 P) {}
uvec4 texelFetch(usampler2DRect sampler, ivec2 P) {}
vec4 texelFetch(sampler1DArray sampler, ivec2 P, int lod) {}
ivec4 texelFetch(isampler1DArray sampler, ivec2 P, int lod) {}
uvec4 texelFetch(usampler1DArray sampler, ivec2 P, int lod) {}
vec4 texelFetch(sampler2DArray sampler, ivec3 P, int lod) {}
ivec4 texelFetch(isampler2DArray sampler, ivec3 P, int lod) {}
uvec4 texelFetch(usampler2DArray sampler, ivec3 P, int lod) {}
vec4 texelFetch(samplerBuffer sampler, int P) {}
ivec4 texelFetch(isamplerBuffer sampler, int P) {}
uvec4 texelFetch(usamplerBuffer sampler, int P) {}
vec4 texelFetch(sampler2DMS sampler, ivec2 P, int sample) {}
ivec4 texelFetch(isampler2DMS sampler, ivec2 P, int sample) {}
uvec4 texelFetch(usampler2DMS sampler, ivec2 P, int sample) {}
vec4 texelFetch(sampler2DMSArray sampler, ivec3 P, int sample) {}
ivec4 texelFetch(isampler2DMSArray sampler, ivec3 P, int sample) {}
uvec4 texelFetch(usampler2DMSArray sampler, ivec3 P, int sample) {}
vec4 texelFetchOffset(sampler1D sampler, int P, int lod, int offset) {}
ivec4 texelFetchOffset(isampler1D sampler, int P, int lod, int offset) {}
uvec4 texelFetchOffset(usampler1D sampler, int P, int lod, int offset) {}
vec4 texelFetchOffset(sampler2D sampler, ivec2 P, int lod, ivec2 offset) {}
ivec4 texelFetchOffset(isampler2D sampler, ivec2 P, int lod, ivec2 offset) {}
uvec4 texelFetchOffset(usampler2D sampler, ivec2 P, int lod, ivec2 offset) {}
vec4 texelFetchOffset(sampler3D sampler, ivec3 P, int lod, ivec3 offset) {}
ivec4 texelFetchOffset(isampler3D sampler, ivec3 P, int lod, ivec3 offset) {}
uvec4 texelFetchOffset(usampler3D sampler, ivec3 P, int lod, ivec3 offset) {}
vec4 texelFetchOffset(sampler2DRect sampler, ivec2 P, ivec2 offset) {}
ivec4 texelFetchOffset(isampler2DRect sampler, ivec2 P, ivec2 offset) {}
uvec4 texelFetchOffset(usampler2DRect sampler, ivec2 P, ivec2 offset) {}
vec4 texelFetchOffset(sampler1DArray sampler, ivec2 P, int lod, int offset) {}
ivec4 texelFetchOffset(isampler1DArray sampler, ivec2 P, int lod, int offset) {}
uvec4 texelFetchOffset(usampler1DArray sampler, ivec2 P, int lod, int offset) {}
vec4 texelFetchOffset(sampler2DArray sampler, ivec3 P, int lod, ivec2 offset) {}
ivec4 texelFetchOffset(isampler2DArray sampler, ivec3 P, int lod, ivec2 offset) {}
uvec4 texelFetchOffset(usampler2DArray sampler, ivec3 P, int lod, ivec2 offset) {}
vec4 textureProjOffset(sampler1D sampler, vec2 P, int offset) {}
ivec4 textureProjOffset(isampler1D sampler, vec2 P, int offset) {}
uvec4 textureProjOffset(usampler1D sampler, vec2 P, int offset) {}
vec4 textureProjOffset(sampler1D sampler, vec2 P, int offset, float bias) {}
ivec4 textureProjOffset(isampler1D sampler, vec2 P, int offset, float bias) {}
uvec4 textureProjOffset(usampler1D sampler, vec2 P, int offset, float bias) {}
vec4 textureProjOffset(sampler1D sampler, vec4 P, int offset) {}
ivec4 textureProjOffset(isampler1D sampler, vec4 P, int offset) {}
uvec4 textureProjOffset(usampler1D sampler, vec4 P, int offset) {}
vec4 textureProjOffset(sampler1D sampler, vec4 P, int offset, float bias) {}
ivec4 textureProjOffset(isampler1D sampler, vec4 P, int offset, float bias) {}
uvec4 textureProjOffset(usampler1D sampler, vec4 P, int offset, float bias) {}
vec4 textureProjOffset(sampler2D sampler, vec3 P, ivec2 offset) {}
ivec4 textureProjOffset(isampler2D sampler, vec3 P, ivec2 offset) {}
uvec4 textureProjOffset(usampler2D sampler, vec3 P, ivec2 offset) {}
vec4 textureProjOffset(sampler2D sampler, vec3 P, ivec2 offset, float bias) {}
ivec4 textureProjOffset(isampler2D sampler, vec3 P, ivec2 offset, float bias) {}
uvec4 textureProjOffset(usampler2D sampler, vec3 P, ivec2 offset, float bias) {}
vec4 textureProjOffset(sampler2D sampler, vec4 P, ivec2 offset) {}
ivec4 textureProjOffset(isampler2D sampler, vec4 P, ivec2 offset) {}
uvec4 textureProjOffset(usampler2D sampler, vec4 P, ivec2 offset) {}
vec4 textureProjOffset(sampler2D sampler, vec4 P, ivec2 offset, float bias) {}
ivec4 textureProjOffset(isampler2D sampler, vec4 P, ivec2 offset, float bias) {}
uvec4 textureProjOffset(usampler2D sampler, vec4 P, ivec2 offset, float bias) {}
vec4 textureProjOffset(sampler3D sampler, vec4 P, ivec3 offset) {}
ivec4 textureProjOffset(isampler3D sampler, vec4 P, ivec3 offset) {}
uvec4 textureProjOffset(usampler3D sampler, vec4 P, ivec3 offset) {}
vec4 textureProjOffset(sampler3D sampler, vec4 P, ivec3 offset, float bias) {}
ivec4 textureProjOffset(isampler3D sampler, vec4 P, ivec3 offset, float bias) {}
uvec4 textureProjOffset(usampler3D sampler, vec4 P, ivec3 offset, float bias) {}
vec4 textureProjOffset(sampler2DRect sampler, vec3 P, ivec2 offset) {}
ivec4 textureProjOffset(isampler2DRect sampler, vec3 P, ivec2 offset) {}
uvec4 textureProjOffset(usampler2DRect sampler, vec3 P, ivec2 offset) {}
vec4 textureProjOffset(sampler2DRect sampler, vec4 P, ivec2 offset) {}
ivec4 textureProjOffset(isampler2DRect sampler, vec4 P, ivec2 offset) {}
uvec4 textureProjOffset(usampler2DRect sampler, vec4 P, ivec2 offset) {}
float textureProjOffset(sampler2DRectShadow sampler, vec4 P, ivec2 offset) {}
float textureProjOffset(sampler1DShadow sampler, vec4 P, int offset) {}
float textureProjOffset(sampler1DShadow sampler, vec4 P, int offset, float bias) {}
float textureProjOffset(sampler2DShadow sampler, vec4 P, ivec2 offset) {}
float textureProjOffset(sampler2DShadow sampler, vec4 P, ivec2 offset, float bias) {}
vec4 textureLodOffset(sampler1D sampler, float P, float lod, int offset) {}
ivec4 textureLodOffset(isampler1D sampler, float P, float lod, int offset) {}
uvec4 textureLodOffset(usampler1D sampler, float P, float lod, int offset) {}
vec4 textureLodOffset(sampler2D sampler, vec2 P, float lod, ivec2 offset) {}
ivec4 textureLodOffset(isampler2D sampler, vec2 P, float lod, ivec2 offset) {}
uvec4 textureLodOffset(usampler2D sampler, vec2 P, float lod, ivec2 offset) {}
vec4 textureLodOffset(sampler3D sampler, vec3 P, float lod, ivec3 offset) {}
ivec4 textureLodOffset(isampler3D sampler, vec3 P, float lod, ivec3 offset) {}
uvec4 textureLodOffset(usampler3D sampler, vec3 P, float lod, ivec3 offset) {}
float textureLodOffset(sampler1DShadow sampler, vec3 P, float lod, int offset) {}
float textureLodOffset(sampler2DShadow sampler, vec3 P, float lod, ivec2 offset) {}
vec4 textureLodOffset(sampler1DArray sampler, vec2 P, float lod, int offset) {}
ivec4 textureLodOffset(isampler1DArray sampler, vec2 P, float lod, int offset) {}
uvec4 textureLodOffset(usampler1DArray sampler, vec2 P, float lod, int offset) {}
vec4 textureLodOffset(sampler2DArray sampler, vec3 P, float lod, ivec2 offset) {}
ivec4 textureLodOffset(isampler2DArray sampler, vec3 P, float lod, ivec2 offset) {}
uvec4 textureLodOffset(usampler2DArray sampler, vec3 P, float lod, ivec2 offset) {}
float textureLodOffset(sampler1DArrayShadow sampler, vec3 P, float lod, int offset) {}
vec4 textureProjLod(sampler1D sampler, vec2 P, float lod) {}
ivec4 textureProjLod(isampler1D sampler, vec2 P, float lod) {}
uvec4 textureProjLod(usampler1D sampler, vec2 P, float lod) {}
vec4 textureProjLod(sampler1D sampler, vec4 P, float lod) {}
ivec4 textureProjLod(isampler1D sampler, vec4 P, float lod) {}
uvec4 textureProjLod(usampler1D sampler, vec4 P, float lod) {}
vec4 textureProjLod(sampler2D sampler, vec3 P, float lod) {}
ivec4 textureProjLod(isampler2D sampler, vec3 P, float lod) {}
uvec4 textureProjLod(usampler2D sampler, vec3 P, float lod) {}
vec4 textureProjLod(sampler2D sampler, vec4 P, float lod) {}
ivec4 textureProjLod(isampler2D sampler, vec4 P, float lod) {}
uvec4 textureProjLod(usampler2D sampler, vec4 P, float lod) {}
vec4 textureProjLod(sampler3D sampler, vec4 P, float lod) {}
ivec4 textureProjLod(isampler3D sampler, vec4 P, float lod) {}
uvec4 textureProjLod(usampler3D sampler, vec4 P, float lod) {}
float textureProjLod(sampler1DShadow sampler, vec4 P, float lod) {}
float textureProjLod(sampler2DShadow sampler, vec4 P, float lod) {}
vec4 textureProjLodOffset(sampler1D sampler, vec2 P, float lod, int offset) {}
ivec4 textureProjLodOffset(isampler1D sampler, vec2 P, float lod, int offset) {}
uvec4 textureProjLodOffset(usampler1D sampler, vec2 P, float lod, int offset) {}
vec4 textureProjLodOffset(sampler1D sampler, vec4 P, float lod, int offset) {}
ivec4 textureProjLodOffset(isampler1D sampler, vec4 P, float lod, int offset) {}
uvec4 textureProjLodOffset(usampler1D sampler, vec4 P, float lod, int offset) {}
vec4 textureProjLodOffset(sampler2D sampler, vec3 P, float lod, ivec2 offset) {}
ivec4 textureProjLodOffset(isampler2D sampler, vec3 P, float lod, ivec2 offset) {}
uvec4 textureProjLodOffset(usampler2D sampler, vec3 P, float lod, ivec2 offset) {}
vec4 textureProjLodOffset(sampler2D sampler, vec4 P, float lod, ivec2 offset) {}
ivec4 textureProjLodOffset(isampler2D sampler, vec4 P, float lod, ivec2 offset) {}
uvec4 textureProjLodOffset(usampler2D sampler, vec4 P, float lod, ivec2 offset) {}
vec4 textureProjLodOffset(sampler3D sampler, vec4 P, float lod, ivec3 offset) {}
ivec4 textureProjLodOffset(isampler3D sampler, vec4 P, float lod, ivec3 offset) {}
uvec4 textureProjLodOffset(usampler3D sampler, vec4 P, float lod, ivec3 offset) {}
float textureProjLodOffset(sampler1DShadow sampler, vec4 P, float lod, int offset) {}
float textureProjLodOffset(sampler2DShadow sampler, vec4 P, float lod, ivec2 offset) {}
vec4 textureGrad(sampler1D sampler, float P, float dPdx, float dPdy) {}
ivec4 textureGrad(isampler1D sampler, float P, float dPdx, float dPdy) {}
uvec4 textureGrad(usampler1D sampler, float P, float dPdx, float dPdy) {}
vec4 textureGrad(sampler2D sampler, vec2 P, vec2 dPdx, vec2 dPdy) {}
ivec4 textureGrad(isampler2D sampler, vec2 P, vec2 dPdx, vec2 dPdy) {}
uvec4 textureGrad(usampler2D sampler, vec2 P, vec2 dPdx, vec2 dPdy) {}
vec4 textureGrad(sampler3D sampler, vec3 P, vec3 dPdx, vec3 dPdy) {}
ivec4 textureGrad(isampler3D sampler, vec3 P, vec3 dPdx, vec3 dPdy) {}
uvec4 textureGrad(usampler3D sampler, vec3 P, vec3 dPdx, vec3 dPdy) {}
vec4 textureGrad(samplerCube sampler, vec3 P, vec3 dPdx, vec3 dPdy) {}
ivec4 textureGrad(isamplerCube sampler, vec3 P, vec3 dPdx, vec3 dPdy) {}
uvec4 textureGrad(usamplerCube sampler, vec3 P, vec3 dPdx, vec3 dPdy) {}
vec4 textureGrad(sampler2DRect sampler, vec2 P, vec2 dPdx, vec2 dPdy) {}
ivec4 textureGrad(isampler2DRect sampler, vec2 P, vec2 dPdx, vec2 dPdy) {}
uvec4 textureGrad(usampler2DRect sampler, vec2 P, vec2 dPdx, vec2 dPdy) {}
float textureGrad(sampler2DRectShadow sampler, vec3 P, vec2 dPdx, vec2 dPdy) {}
float textureGrad(sampler1DShadow sampler, vec3 P, float dPdx, float dPdy) {}
vec4 textureGrad(sampler1DArray sampler, vec2 P, float dPdx, float dPdy) {}
ivec4 textureGrad(isampler1DArray sampler, vec2 P, float dPdx, float dPdy) {}
uvec4 textureGrad(usampler1DArray sampler, vec2 P, float dPdx, float dPdy) {}
vec4 textureGrad(sampler2DArray sampler, vec3 P, vec2 dPdx, vec2 dPdy) {}
ivec4 textureGrad(isampler2DArray sampler, vec3 P, vec2 dPdx, vec2 dPdy) {}
uvec4 textureGrad(usampler2DArray sampler, vec3 P, vec2 dPdx, vec2 dPdy) {}
float textureGrad(sampler1DArrayShadow sampler, vec3 P, float dPdx, float dPdy) {}
float textureGrad(sampler2DShadow sampler, vec3 P, vec2 dPdx, vec2 dPdy) {}
float textureGrad(samplerCubeShadow sampler, vec4 P, vec3 dPdx, vec3 dPdy) {}
float textureGrad(sampler2DArrayShadow sampler, vec4 P, vec2 dPdx, vec2 dPdy) {}
vec4 textureGrad(samplerCubeArray sampler, vec4 P, vec3 dPdx, vec3 dPdy) {}
ivec4 textureGrad(isamplerCubeArray sampler, vec4 P, vec3 dPdx, vec3 dPdy) {}
uvec4 textureGrad(usamplerCubeArray sampler, vec4 P, vec3 dPdx, vec3 dPdy) {}
vec4 textureGradOffset(sampler1D sampler, float P, float dPdx, float dPdy, int offset) {}
ivec4 textureGradOffset(isampler1D sampler, float P, float dPdx, float dPdy, int offset) {}
uvec4 textureGradOffset(usampler1D sampler, float P, float dPdx, float dPdy, int offset) {}
vec4 textureGradOffset(sampler2D sampler, vec2 P, vec2 dPdx, vec2 dPdy, ivec2 offset) {}
ivec4 textureGradOffset(isampler2D sampler, vec2 P, vec2 dPdx, vec2 dPdy, ivec2 offset) {}
uvec4 textureGradOffset(usampler2D sampler, vec2 P, vec2 dPdx, vec2 dPdy, ivec2 offset) {}
vec4 textureGradOffset(sampler3D sampler, vec3 P, vec3 dPdx, vec3 dPdy, ivec3 offset) {}
ivec4 textureGradOffset(isampler3D sampler, vec3 P, vec3 dPdx, vec3 dPdy, ivec3 offset) {}
uvec4 textureGradOffset(usampler3D sampler, vec3 P, vec3 dPdx, vec3 dPdy, ivec3 offset) {}
vec4 textureGradOffset(sampler2DRect sampler, vec2 P, vec2 dPdx, vec2 dPdy, ivec2 offset) {}
ivec4 textureGradOffset(isampler2DRect sampler, vec2 P, vec2 dPdx, vec2 dPdy, ivec2 offset) {}
uvec4 textureGradOffset(usampler2DRect sampler, vec2 P, vec2 dPdx, vec2 dPdy, ivec2 offset) {}
float textureGradOffset(sampler2DRectShadow sampler, vec3 P, vec2 dPdx, vec2 dPdy, ivec2 offset) {}
float textureGradOffset(sampler1DShadow sampler, vec3 P, float dPdx, float dPdy, int offset) {}
float textureGradOffset(sampler2DShadow sampler, vec3 P, vec2 dPdx, vec2 dPdy, ivec2 offset) {}
vec4 textureGradOffset(sampler2DArray sampler, vec3 P, vec2 dPdx, vec2 dPdy, ivec2 offset) {}
ivec4 textureGradOffset(isampler2DArray sampler, vec3 P, vec2 dPdx, vec2 dPdy, ivec2 offset) {}
uvec4 textureGradOffset(usampler2DArray sampler, vec3 P, vec2 dPdx, vec2 dPdy, ivec2 offset) {}
vec4 textureGradOffset(sampler1DArray sampler, vec2 P, float dPdx, float dPdy, int offset) {}
ivec4 textureGradOffset(isampler1DArray sampler, vec2 P, float dPdx, float dPdy, int offset) {}
uvec4 textureGradOffset(usampler1DArray sampler, vec2 P, float dPdx, float dPdy, int offset) {}
float textureGradOffset(sampler1DArrayShadow sampler, vec3 P, float dPdx, float dPdy, int offset) {}
float textureGradOffset(sampler2DArrayShadow sampler, vec4 P, vec2 dPdx, vec2 dPdy, ivec2 offset) {}
vec4 textureProjGrad(sampler1D sampler, vec2 P, float dPdx, float dPdy) {}
ivec4 textureProjGrad(isampler1D sampler, vec2 P, float dPdx, float dPdy) {}
uvec4 textureProjGrad(usampler1D sampler, vec2 P, float dPdx, float dPdy) {}
vec4 textureProjGrad(sampler1D sampler, vec4 P, float dPdx, float dPdy) {}
ivec4 textureProjGrad(isampler1D sampler, vec4 P, float dPdx, float dPdy) {}
uvec4 textureProjGrad(usampler1D sampler, vec4 P, float dPdx, float dPdy) {}
vec4 textureProjGrad(sampler2D sampler, vec3 P, vec2 dPdx, vec2 dPdy) {}
ivec4 textureProjGrad(isampler2D sampler, vec3 P, vec2 dPdx, vec2 dPdy) {}
uvec4 textureProjGrad(usampler2D sampler, vec3 P, vec2 dPdx, vec2 dPdy) {}
vec4 textureProjGrad(sampler2D sampler, vec4 P, vec2 dPdx, vec2 dPdy) {}
ivec4 textureProjGrad(isampler2D sampler, vec4 P, vec2 dPdx, vec2 dPdy) {}
uvec4 textureProjGrad(usampler2D sampler, vec4 P, vec2 dPdx, vec2 dPdy) {}
vec4 textureProjGrad(sampler3D sampler, vec4 P, vec3 dPdx, vec3 dPdy) {}
ivec4 textureProjGrad(isampler3D sampler, vec4 P, vec3 dPdx, vec3 dPdy) {}
uvec4 textureProjGrad(usampler3D sampler, vec4 P, vec3 dPdx, vec3 dPdy) {}
vec4 textureProjGrad(sampler2DRect sampler, vec3 P, vec2 dPdx, vec2 dPdy) {}
ivec4 textureProjGrad(isampler2DRect sampler, vec3 P, vec2 dPdx, vec2 dPdy) {}
uvec4 textureProjGrad(usampler2DRect sampler, vec3 P, vec2 dPdx, vec2 dPdy) {}
vec4 textureProjGrad(sampler2DRect sampler, vec4 P, vec2 dPdx, vec2 dPdy) {}
ivec4 textureProjGrad(isampler2DRect sampler, vec4 P, vec2 dPdx, vec2 dPdy) {}
uvec4 textureProjGrad(usampler2DRect sampler, vec4 P, vec2 dPdx, vec2 dPdy) {}
float textureProjGrad(sampler2DRectShadow sampler, vec4 P, vec2 dPdx, vec2 dPdy) {}
float textureProjGrad(sampler1DShadow sampler, vec4 P, float dPdx, float dPdy) {}
float textureProjGrad(sampler2DShadow sampler, vec4 P, vec2 dPdx, vec2 dPdy) {}
vec4 textureProjGradOffset(sampler1D sampler, vec2 P, float dPdx, float dPdy, int offset) {}
ivec4 textureProjGradOffset(isampler1D sampler, vec2 P, float dPdx, float dPdy, int offset) {}
uvec4 textureProjGradOffset(usampler1D sampler, vec2 P, float dPdx, float dPdy, int offset) {}
vec4 textureProjGradOffset(sampler1D sampler, vec4 P, float dPdx, float dPdy, int offset) {}
ivec4 textureProjGradOffset(isampler1D sampler, vec4 P, float dPdx, float dPdy, int offset) {}
uvec4 textureProjGradOffset(usampler1D sampler, vec4 P, float dPdx, float dPdy, int offset) {}
vec4 textureProjGradOffset(sampler2D sampler, vec3 P, vec2 dPdx, vec2 dPdy, ivec2 offset) {}
ivec4 textureProjGradOffset(isampler2D sampler, vec3 P, vec2 dPdx, vec2 dPdy, ivec2 offset) {}
uvec4 textureProjGradOffset(usampler2D sampler, vec3 P, vec2 dPdx, vec2 dPdy, ivec2 offset) {}
vec4 textureProjGradOffset(sampler2D sampler, vec4 P, vec2 dPdx, vec2 dPdy, ivec2 offset) {}
ivec4 textureProjGradOffset(isampler2D sampler, vec4 P, vec2 dPdx, vec2 dPdy, ivec2 offset) {}
uvec4 textureProjGradOffset(usampler2D sampler, vec4 P, vec2 dPdx, vec2 dPdy, ivec2 offset) {}
vec4 textureProjGradOffset(sampler3D sampler, vec4 P, vec3 dPdx, vec3 dPdy, ivec3 offset) {}
ivec4 textureProjGradOffset(isampler3D sampler, vec4 P, vec3 dPdx, vec3 dPdy, ivec3 offset) {}
uvec4 textureProjGradOffset(usampler3D sampler, vec4 P, vec3 dPdx, vec3 dPdy, ivec3 offset) {}
vec4 textureProjGradOffset(sampler2DRect sampler, vec3 P, vec2 dPdx, vec2 dPdy, ivec2 offset) {}
ivec4 textureProjGradOffset(isampler2DRect sampler, vec3 P, vec2 dPdx, vec2 dPdy, ivec2 offset) {}
uvec4 textureProjGradOffset(usampler2DRect sampler, vec3 P, vec2 dPdx, vec2 dPdy, ivec2 offset) {}
vec4 textureProjGradOffset(sampler2DRect sampler, vec4 P, vec2 dPdx, vec2 dPdy, ivec2 offset) {}
ivec4 textureProjGradOffset(isampler2DRect sampler, vec4 P, vec2 dPdx, vec2 dPdy, ivec2 offset) {}
uvec4 textureProjGradOffset(usampler2DRect sampler, vec4 P, vec2 dPdx, vec2 dPdy, ivec2 offset) {}
float textureProjGradOffset(sampler2DRectShadow sampler, vec4 P, vec2 dPdx, vec2 dPdy, ivec2 offset) {}
float textureProjGradOffset(sampler1DShadow sampler, vec4 P, float dPdx, float dPdy, int offset) {}
float textureProjGradOffset(sampler2DShadow sampler, vec4 P, vec2 dPdx, vec2 dPdy, ivec2 offset) {}
vec4 textureGather(sampler2D sampler, vec2 P) {}
ivec4 textureGather(isampler2D sampler, vec2 P) {}
uvec4 textureGather(usampler2D sampler, vec2 P) {}
vec4 textureGather(sampler2D sampler, vec2 P, int comp) {}
ivec4 textureGather(isampler2D sampler, vec2 P, int comp) {}
uvec4 textureGather(usampler2D sampler, vec2 P, int comp) {}
vec4 textureGather(sampler2DArray sampler, vec3 P) {}
ivec4 textureGather(isampler2DArray sampler, vec3 P) {}
uvec4 textureGather(usampler2DArray sampler, vec3 P) {}
vec4 textureGather(sampler2DArray sampler, vec3 P, int comp) {}
ivec4 textureGather(isampler2DArray sampler, vec3 P, int comp) {}
uvec4 textureGather(usampler2DArray sampler, vec3 P, int comp) {}
vec4 textureGather(samplerCube sampler, vec3 P) {}
ivec4 textureGather(isamplerCube sampler, vec3 P) {}
uvec4 textureGather(usamplerCube sampler, vec3 P) {}
vec4 textureGather(samplerCube sampler, vec3 P, int comp) {}
ivec4 textureGather(isamplerCube sampler, vec3 P, int comp) {}
uvec4 textureGather(usamplerCube sampler, vec3 P, int comp) {}
vec4 textureGather(samplerCubeArray sampler, vec4 P) {}
ivec4 textureGather(isamplerCubeArray sampler, vec4 P) {}
uvec4 textureGather(usamplerCubeArray sampler, vec4 P) {}
vec4 textureGather(samplerCubeArray sampler, vec4 P, int comp) {}
ivec4 textureGather(isamplerCubeArray sampler, vec4 P, int comp) {}
uvec4 textureGather(usamplerCubeArray sampler, vec4 P, int comp) {}
vec4 textureGather(sampler2DRect sampler, vec2 P) {}
ivec4 textureGather(isampler2DRect sampler, vec2 P) {}
uvec4 textureGather(usampler2DRect sampler, vec2 P) {}
vec4 textureGather(sampler2DRect sampler, vec2 P, int comp) {}
ivec4 textureGather(isampler2DRect sampler, vec2 P, int comp) {}
uvec4 textureGather(usampler2DRect sampler, vec2 P, int comp) {}
vec4 textureGather(sampler2DShadow sampler, vec2 P, float refZ) {}
vec4 textureGather(sampler2DArrayShadow sampler, vec3 P, float refZ) {}
vec4 textureGather(samplerCubeShadow sampler, vec3 P, float refZ) {}
vec4 textureGather(samplerCubeArrayShadow sampler, vec4 P, float refZ) {}
vec4 textureGather(sampler2DRectShadow sampler, vec2 P, float refZ) {}
vec4 textureGatherOffset(sampler2D sampler, vec2 P, ivec2 offset) {}
ivec4 textureGatherOffset(isampler2D sampler, vec2 P, ivec2 offset) {}
uvec4 textureGatherOffset(usampler2D sampler, vec2 P, ivec2 offset) {}
vec4 textureGatherOffset(sampler2D sampler, vec2 P, ivec2 offset, int comp) {}
ivec4 textureGatherOffset(isampler2D sampler, vec2 P, ivec2 offset, int comp) {}
uvec4 textureGatherOffset(usampler2D sampler, vec2 P, ivec2 offset, int comp) {}
vec4 textureGatherOffset(sampler2DArray sampler, vec3 P, ivec2 offset) {}
ivec4 textureGatherOffset(isampler2DArray sampler, vec3 P, ivec2 offset) {}
uvec4 textureGatherOffset(usampler2DArray sampler, vec3 P, ivec2 offset) {}
vec4 textureGatherOffset(sampler2DArray sampler, vec3 P, ivec2 offset, int comp) {}
ivec4 textureGatherOffset(isampler2DArray sampler, vec3 P, ivec2 offset, int comp) {}
uvec4 textureGatherOffset(usampler2DArray sampler, vec3 P, ivec2 offset, int comp) {}
vec4 textureGatherOffset(sampler2DShadow sampler, vec2 P, float refZ, ivec2 offset) {}
vec4 textureGatherOffset(sampler2DArrayShadow sampler, vec3 P, float refZ, ivec2 offset) {}
vec4 textureGatherOffset(sampler2DRect sampler, vec2 P, ivec2 offset) {}
ivec4 textureGatherOffset(isampler2DRect sampler, vec2 P, ivec2 offset) {}
uvec4 textureGatherOffset(usampler2DRect sampler, vec2 P, ivec2 offset) {}
vec4 textureGatherOffset(sampler2DRect sampler, vec2 P, ivec2 offset, int comp) {}
ivec4 textureGatherOffset(isampler2DRect sampler, vec2 P, ivec2 offset, int comp) {}
uvec4 textureGatherOffset(usampler2DRect sampler, vec2 P, ivec2 offset, int comp) {}
vec4 textureGatherOffset(sampler2DRectShadow sampler, vec2 P, float refZ, ivec2 offset) {}
vec4 textureGatherOffsets(sampler2D sampler, vec2 P, ivec2[4] offsets) {}
ivec4 textureGatherOffsets(isampler2D sampler, vec2 P, ivec2[4] offsets) {}
uvec4 textureGatherOffsets(usampler2D sampler, vec2 P, ivec2[4] offsets) {}
vec4 textureGatherOffsets(sampler2D sampler, vec2 P, ivec2[4] offsets, int comp) {}
ivec4 textureGatherOffsets(isampler2D sampler, vec2 P, ivec2[4] offsets, int comp) {}
uvec4 textureGatherOffsets(usampler2D sampler, vec2 P, ivec2[4] offsets, int comp) {}
vec4 textureGatherOffsets(sampler2DArray sampler, vec3 P, ivec2[4] offsets) {}
ivec4 textureGatherOffsets(isampler2DArray sampler, vec3 P, ivec2[4] offsets) {}
uvec4 textureGatherOffsets(usampler2DArray sampler, vec3 P, ivec2[4] offsets) {}
vec4 textureGatherOffsets(sampler2DArray sampler, vec3 P, ivec2[4] offsets, int comp) {}
ivec4 textureGatherOffsets(isampler2DArray sampler, vec3 P, ivec2[4] offsets, int comp) {}
uvec4 textureGatherOffsets(usampler2DArray sampler, vec3 P, ivec2[4] offsets, int comp) {}
vec4 textureGatherOffsets(sampler2DShadow sampler, vec2 P, float refZ, ivec2[4] offsets) {}
vec4 textureGatherOffsets(sampler2DArrayShadow sampler, vec3 P, float refZ, ivec2[4] offsets) {}
vec4 textureGatherOffsets(sampler2DRect sampler, vec2 P, ivec2[4] offsets) {}
ivec4 textureGatherOffsets(isampler2DRect sampler, vec2 P, ivec2[4] offsets) {}
uvec4 textureGatherOffsets(usampler2DRect sampler, vec2 P, ivec2[4] offsets) {}
vec4 textureGatherOffsets(sampler2DRect sampler, vec2 P, ivec2[4] offsets, int comp) {}
ivec4 textureGatherOffsets(isampler2DRect sampler, vec2 P, ivec2[4] offsets, int comp) {}
uvec4 textureGatherOffsets(usampler2DRect sampler, vec2 P, ivec2[4] offsets, int comp) {}
vec4 textureGatherOffsets(sampler2DRectShadow sampler, vec2 P, float refZ, ivec2[4] offsets) {}
uint atomicCounterIncrement(atomic_uint c) {}
uint atomicCounterDecrement(atomic_uint c) {}
uint atomicCounter(atomic_uint c) {}
uint atomicCounterAdd(atomic_uint c, uint data) {}
uint atomicCounterSubtract(atomic_uint c, uint data) {}
uint atomicCounterMin(atomic_uint c, uint data) {}
uint atomicCounterMax(atomic_uint c, uint data) {}
uint atomicCounterAnd(atomic_uint c, uint data) {}
uint atomicCounterOr(atomic_uint c, uint data) {}
uint atomicCounterXor(atomic_uint c, uint data)  {}
uint atomicCounterXor(atomic_uint c, uint data) {}
uint atomicCounterCompSwap(atomic_uint c, uint compare, uint data) {}
uint atomicAdd(uint mem, uint data) {}
int atomicAdd(int mem, int data) {}
uint atomicMin(uint mem, uint data) {}
int atomicMin(int mem, int data) {}
uint atomicMax(uint mem, uint data) {}
int atomicMax(int mem, int data) {}
uint atomicAnd(uint mem, uint data) {}
int atomicAnd(int mem, int data) {}
uint atomicOr(uint mem, uint data) {}
int atomicOr(int mem, int data) {}
uint atomicXor(uint mem, uint data) {}
int atomicXor(int mem, int data) {}
uint atomicExchange(uint mem, uint data) {}
int atomicExchange(int mem, int data) {}
uint atomicCompSwap(uint mem, uint compare, uint data) {}
int atomicCompSwap(int mem, int compare, int data) {}
int imageSize(image1D image) {}
int imageSize(iimage1D image) {}
int imageSize(uimage1D image) {}
ivec2 imageSize(image2D image) {}
ivec2 imageSize(iimage2D image) {}
ivec2 imageSize(uimage2D image) {}
ivec3 imageSize(image3D image) {}
ivec3 imageSize(iimage3D image) {}
ivec3 imageSize(uimage3D image) {}
ivec2 imageSize(imageCube image) {}
ivec2 imageSize(iimageCube image) {}
ivec2 imageSize(uimageCube image) {}
ivec3 imageSize(imageCubeArray image) {}
ivec3 imageSize(iimageCubeArray image) {}
ivec3 imageSize(uimageCubeArray image) {}
ivec3 imageSize(image2DArray image) {}
ivec3 imageSize(iimage2DArray image) {}
ivec3 imageSize(uimage2DArray image) {}
ivec2 imageSize(image1DArray image) {}
ivec2 imageSize(iimage1DArray image) {}
ivec2 imageSize(uimage1DArray image) {}
ivec2 imageSize(image2DMS image) {}
ivec2 imageSize(iimage2DMS image) {}
ivec2 imageSize(uimage2DMS image) {}
ivec3 imageSize(image2DMSArray image) {}
ivec3 imageSize(iimage2DMSArray image) {}
ivec3 imageSize(uimage2DMSArray image) {}
int imageSize(imageBuffer image) {}
int imageSize(iimageBuffer image) {}
int imageSize(uimageBuffer image) {}
int imageSamples(image2DMS image) {}
int imageSamples(iimage2DMS image) {}
int imageSamples(uimage2DMS image) {}
int imageSamples(image2DMSArray image) {}
int imageSamples(iimage2DMSArray image) {}
int imageSamples(uimage2DMSArray image) {}
vec4 imageLoad(image2D image, ivec2 P) {}
ivec4 imageLoad(iimage2D image, ivec2 P) {}
uvec4 imageLoad(uimage2D image, ivec2 P) {}
vec4 imageLoad(image3D image, ivec3 P) {}
ivec4 imageLoad(iimage3D image, ivec3 P) {}
uvec4 imageLoad(uimage3D image, ivec3 P) {}
vec4 imageLoad(imageCube image, ivec3 P) {}
ivec4 imageLoad(iimageCube image, ivec3 P) {}
uvec4 imageLoad(uimageCube image, ivec3 P) {}
vec4 imageLoad(imageBuffer image, int P) {}
ivec4 imageLoad(iimageBuffer image, int P) {}
uvec4 imageLoad(uimageBuffer image, int P) {}
vec4 imageLoad(image2DArray image, ivec3 P) {}
ivec4 imageLoad(iimage2DArray image, ivec3 P) {}
uvec4 imageLoad(uimage2DArray image, ivec3 P) {}
vec4 imageLoad(imageCubeArray image, ivec3 P) {}
ivec4 imageLoad(iimageCubeArray image, ivec3 P) {}
uvec4 imageLoad(uimageCubeArray image, ivec3 P) {}
vec4 imageLoad(image1D image, int P) {}
ivec4 imageLoad(iimage1D image, int P) {}
uvec4 imageLoad(uimage1D image, int P) {}
vec4 imageLoad(image1DArray image, ivec2 P) {}
ivec4 imageLoad(iimage1DArray image, ivec2 P) {}
uvec4 imageLoad(uimage1DArray image, ivec2 P) {}
vec4 imageLoad(image2DMS image, ivec2 P, int sample) {}
ivec4 imageLoad(iimage2DMS image, ivec2 P, int sample) {}
uvec4 imageLoad(uimage2DMS image, ivec2 P, int sample) {}
vec4 imageLoad(image2DMSArray image, ivec3 P, int sample) {}
ivec4 imageLoad(iimage2DMSArray image, ivec3 P, int sample) {}
uvec4 imageLoad(uimage2DMSArray image, ivec3 P, int sample) {}
void imageStore(vec4 image, ivec2 P, image2D data) {}
void imageStore(ivec4 image, ivec2 P, iimage2D data) {}
void imageStore(uvec4 image, ivec2 P, uimage2D data) {}
void imageStore(vec4 image, ivec3 P, image3D data) {}
void imageStore(ivec4 image, ivec3 P, iimage3D data) {}
void imageStore(uvec4 image, ivec3 P, uimage3D data) {}
void imageStore(vec4 image, ivec3 P, imageCube data) {}
void imageStore(ivec4 image, ivec3 P, iimageCube data) {}
void imageStore(uvec4 image, ivec3 P, uimageCube data) {}
void imageStore(vec4 image, int P, imageBuffer data) {}
void imageStore(ivec4 image, int P, iimageBuffer data) {}
void imageStore(uvec4 image, int P, uimageBuffer data) {}
void imageStore(vec4 image, ivec3 P, image2DArray data) {}
void imageStore(ivec4 image, ivec3 P, iimage2DArray data) {}
void imageStore(uvec4 image, ivec3 P, uimage2DArray data) {}
void imageStore(vec4 image, ivec3 P, imageCubeArray data) {}
void imageStore(ivec4 image, ivec3 P, iimageCubeArray data) {}
void imageStore(uvec4 image, ivec3 P, uimageCubeArray data) {}
void imageStore(vec4 image, int P, image1D data) {}
void imageStore(ivec4 image, int P, iimage1D data) {}
void imageStore(uvec4 image, int P, uimage1D data) {}
void imageStore(vec4 image, ivec2 P, image1DArray data) {}
void imageStore(ivec4 image, ivec2 P, iimage1DArray data) {}
void imageStore(uvec4 image, ivec2 P, uimage1DArray data) {}
void imageStore(vec4 image, ivec2 P, int sample, image2DMS data) {}
void imageStore(ivec4 image, ivec2 P, int sample, iimage2DMS data) {}
void imageStore(uvec4 image, ivec2 P, int sample, uimage2DMS data) {}
void imageStore(vec4 image, ivec3 P, int sample, image2DMSArray data) {}
void imageStore(ivec4 image, ivec3 P, int sample, iimage2DMSArray data) {}
void imageStore(uvec4 image, ivec3 P, int sample, uimage2DMSArray data) {}
uint imageAtomicAdd(image2D image, ivec2 P, uint data) {}
uint imageAtomicAdd(iimage2D image, ivec2 P, uint data) {}
uint imageAtomicAdd(uimage2D image, ivec2 P, uint data) {}
uint imageAtomicAdd(image3D image, ivec3 P, uint data) {}
uint imageAtomicAdd(iimage3D image, ivec3 P, uint data) {}
uint imageAtomicAdd(uimage3D image, ivec3 P, uint data) {}
uint imageAtomicAdd(imageCube image, ivec3 P, uint data) {}
uint imageAtomicAdd(iimageCube image, ivec3 P, uint data) {}
uint imageAtomicAdd(uimageCube image, ivec3 P, uint data) {}
uint imageAtomicAdd(imageBuffer image, int P, uint data) {}
uint imageAtomicAdd(iimageBuffer image, int P, uint data) {}
uint imageAtomicAdd(uimageBuffer image, int P, uint data) {}
uint imageAtomicAdd(image2DArray image, ivec3 P, uint data) {}
uint imageAtomicAdd(iimage2DArray image, ivec3 P, uint data) {}
uint imageAtomicAdd(uimage2DArray image, ivec3 P, uint data) {}
uint imageAtomicAdd(imageCubeArray image, ivec3 P, uint data) {}
uint imageAtomicAdd(iimageCubeArray image, ivec3 P, uint data) {}
uint imageAtomicAdd(uimageCubeArray image, ivec3 P, uint data) {}
uint imageAtomicAdd(image1D image, int P, uint data) {}
uint imageAtomicAdd(iimage1D image, int P, uint data) {}
uint imageAtomicAdd(uimage1D image, int P, uint data) {}
uint imageAtomicAdd(image1DArray image, ivec2 P, uint data) {}
uint imageAtomicAdd(iimage1DArray image, ivec2 P, uint data) {}
uint imageAtomicAdd(uimage1DArray image, ivec2 P, uint data) {}
uint imageAtomicAdd(image2DMS image, ivec2 P, int sample, uint data) {}
uint imageAtomicAdd(iimage2DMS image, ivec2 P, int sample, uint data) {}
uint imageAtomicAdd(uimage2DMS image, ivec2 P, int sample, uint data) {}
uint imageAtomicAdd(image2DMSArray image, ivec3 P, int sample, uint data) {}
uint imageAtomicAdd(iimage2DMSArray image, ivec3 P, int sample, uint data) {}
uint imageAtomicAdd(uimage2DMSArray image, ivec3 P, int sample, uint data) {}
int imageAtomicAdd(image2D image, ivec2 P, int data) {}
int imageAtomicAdd(iimage2D image, ivec2 P, int data) {}
int imageAtomicAdd(uimage2D image, ivec2 P, int data) {}
int imageAtomicAdd(image3D image, ivec3 P, int data) {}
int imageAtomicAdd(iimage3D image, ivec3 P, int data) {}
int imageAtomicAdd(uimage3D image, ivec3 P, int data) {}
int imageAtomicAdd(imageCube image, ivec3 P, int data) {}
int imageAtomicAdd(iimageCube image, ivec3 P, int data) {}
int imageAtomicAdd(uimageCube image, ivec3 P, int data) {}
int imageAtomicAdd(imageBuffer image, int P, int data) {}
int imageAtomicAdd(iimageBuffer image, int P, int data) {}
int imageAtomicAdd(uimageBuffer image, int P, int data) {}
int imageAtomicAdd(image2DArray image, ivec3 P, int data) {}
int imageAtomicAdd(iimage2DArray image, ivec3 P, int data) {}
int imageAtomicAdd(uimage2DArray image, ivec3 P, int data) {}
int imageAtomicAdd(imageCubeArray image, ivec3 P, int data) {}
int imageAtomicAdd(iimageCubeArray image, ivec3 P, int data) {}
int imageAtomicAdd(uimageCubeArray image, ivec3 P, int data) {}
int imageAtomicAdd(image1D image, int P, int data) {}
int imageAtomicAdd(iimage1D image, int P, int data) {}
int imageAtomicAdd(uimage1D image, int P, int data) {}
int imageAtomicAdd(image1DArray image, ivec2 P, int data) {}
int imageAtomicAdd(iimage1DArray image, ivec2 P, int data) {}
int imageAtomicAdd(uimage1DArray image, ivec2 P, int data) {}
int imageAtomicAdd(image2DMS image, ivec2 P, int sample, int data) {}
int imageAtomicAdd(iimage2DMS image, ivec2 P, int sample, int data) {}
int imageAtomicAdd(uimage2DMS image, ivec2 P, int sample, int data) {}
int imageAtomicAdd(image2DMSArray image, ivec3 P, int sample, int data) {}
int imageAtomicAdd(iimage2DMSArray image, ivec3 P, int sample, int data) {}
int imageAtomicAdd(uimage2DMSArray image, ivec3 P, int sample, int data) {}
uint imageAtomicMin(image2D image, ivec2 P, uint data) {}
uint imageAtomicMin(iimage2D image, ivec2 P, uint data) {}
uint imageAtomicMin(uimage2D image, ivec2 P, uint data) {}
uint imageAtomicMin(image3D image, ivec3 P, uint data) {}
uint imageAtomicMin(iimage3D image, ivec3 P, uint data) {}
uint imageAtomicMin(uimage3D image, ivec3 P, uint data) {}
uint imageAtomicMin(imageCube image, ivec3 P, uint data) {}
uint imageAtomicMin(iimageCube image, ivec3 P, uint data) {}
uint imageAtomicMin(uimageCube image, ivec3 P, uint data) {}
uint imageAtomicMin(imageBuffer image, int P, uint data) {}
uint imageAtomicMin(iimageBuffer image, int P, uint data) {}
uint imageAtomicMin(uimageBuffer image, int P, uint data) {}
uint imageAtomicMin(image2DArray image, ivec3 P, uint data) {}
uint imageAtomicMin(iimage2DArray image, ivec3 P, uint data) {}
uint imageAtomicMin(uimage2DArray image, ivec3 P, uint data) {}
uint imageAtomicMin(imageCubeArray image, ivec3 P, uint data) {}
uint imageAtomicMin(iimageCubeArray image, ivec3 P, uint data) {}
uint imageAtomicMin(uimageCubeArray image, ivec3 P, uint data) {}
uint imageAtomicMin(image1D image, int P, uint data) {}
uint imageAtomicMin(iimage1D image, int P, uint data) {}
uint imageAtomicMin(uimage1D image, int P, uint data) {}
uint imageAtomicMin(image1DArray image, ivec2 P, uint data) {}
uint imageAtomicMin(iimage1DArray image, ivec2 P, uint data) {}
uint imageAtomicMin(uimage1DArray image, ivec2 P, uint data) {}
uint imageAtomicMin(image2DMS image, ivec2 P, int sample, uint data) {}
uint imageAtomicMin(iimage2DMS image, ivec2 P, int sample, uint data) {}
uint imageAtomicMin(uimage2DMS image, ivec2 P, int sample, uint data) {}
uint imageAtomicMin(image2DMSArray image, ivec3 P, int sample, uint data) {}
uint imageAtomicMin(iimage2DMSArray image, ivec3 P, int sample, uint data) {}
uint imageAtomicMin(uimage2DMSArray image, ivec3 P, int sample, uint data) {}
int imageAtomicMin(image2D image, ivec2 P, int data) {}
int imageAtomicMin(iimage2D image, ivec2 P, int data) {}
int imageAtomicMin(uimage2D image, ivec2 P, int data) {}
int imageAtomicMin(image3D image, ivec3 P, int data) {}
int imageAtomicMin(iimage3D image, ivec3 P, int data) {}
int imageAtomicMin(uimage3D image, ivec3 P, int data) {}
int imageAtomicMin(imageCube image, ivec3 P, int data) {}
int imageAtomicMin(iimageCube image, ivec3 P, int data) {}
int imageAtomicMin(uimageCube image, ivec3 P, int data) {}
int imageAtomicMin(imageBuffer image, int P, int data) {}
int imageAtomicMin(iimageBuffer image, int P, int data) {}
int imageAtomicMin(uimageBuffer image, int P, int data) {}
int imageAtomicMin(image2DArray image, ivec3 P, int data) {}
int imageAtomicMin(iimage2DArray image, ivec3 P, int data) {}
int imageAtomicMin(uimage2DArray image, ivec3 P, int data) {}
int imageAtomicMin(imageCubeArray image, ivec3 P, int data) {}
int imageAtomicMin(iimageCubeArray image, ivec3 P, int data) {}
int imageAtomicMin(uimageCubeArray image, ivec3 P, int data) {}
int imageAtomicMin(image1D image, int P, int data) {}
int imageAtomicMin(iimage1D image, int P, int data) {}
int imageAtomicMin(uimage1D image, int P, int data) {}
int imageAtomicMin(image1DArray image, ivec2 P, int data) {}
int imageAtomicMin(iimage1DArray image, ivec2 P, int data) {}
int imageAtomicMin(uimage1DArray image, ivec2 P, int data) {}
int imageAtomicMin(image2DMS image, ivec2 P, int sample, int data) {}
int imageAtomicMin(iimage2DMS image, ivec2 P, int sample, int data) {}
int imageAtomicMin(uimage2DMS image, ivec2 P, int sample, int data) {}
int imageAtomicMin(image2DMSArray image, ivec3 P, int sample, int data) {}
int imageAtomicMin(iimage2DMSArray image, ivec3 P, int sample, int data) {}
int imageAtomicMin(uimage2DMSArray image, ivec3 P, int sample, int data) {}
uint imageAtomicMax(image2D image, ivec2 P, uint data) {}
uint imageAtomicMax(iimage2D image, ivec2 P, uint data) {}
uint imageAtomicMax(uimage2D image, ivec2 P, uint data) {}
uint imageAtomicMax(image3D image, ivec3 P, uint data) {}
uint imageAtomicMax(iimage3D image, ivec3 P, uint data) {}
uint imageAtomicMax(uimage3D image, ivec3 P, uint data) {}
uint imageAtomicMax(imageCube image, ivec3 P, uint data) {}
uint imageAtomicMax(iimageCube image, ivec3 P, uint data) {}
uint imageAtomicMax(uimageCube image, ivec3 P, uint data) {}
uint imageAtomicMax(imageBuffer image, int P, uint data) {}
uint imageAtomicMax(iimageBuffer image, int P, uint data) {}
uint imageAtomicMax(uimageBuffer image, int P, uint data) {}
uint imageAtomicMax(image2DArray image, ivec3 P, uint data) {}
uint imageAtomicMax(iimage2DArray image, ivec3 P, uint data) {}
uint imageAtomicMax(uimage2DArray image, ivec3 P, uint data) {}
uint imageAtomicMax(imageCubeArray image, ivec3 P, uint data) {}
uint imageAtomicMax(iimageCubeArray image, ivec3 P, uint data) {}
uint imageAtomicMax(uimageCubeArray image, ivec3 P, uint data) {}
uint imageAtomicMax(image1D image, int P, uint data) {}
uint imageAtomicMax(iimage1D image, int P, uint data) {}
uint imageAtomicMax(uimage1D image, int P, uint data) {}
uint imageAtomicMax(image1DArray image, ivec2 P, uint data) {}
uint imageAtomicMax(iimage1DArray image, ivec2 P, uint data) {}
uint imageAtomicMax(uimage1DArray image, ivec2 P, uint data) {}
uint imageAtomicMax(image2DMS image, ivec2 P, int sample, uint data) {}
uint imageAtomicMax(iimage2DMS image, ivec2 P, int sample, uint data) {}
uint imageAtomicMax(uimage2DMS image, ivec2 P, int sample, uint data) {}
uint imageAtomicMax(image2DMSArray image, ivec3 P, int sample, uint data) {}
uint imageAtomicMax(iimage2DMSArray image, ivec3 P, int sample, uint data) {}
uint imageAtomicMax(uimage2DMSArray image, ivec3 P, int sample, uint data) {}
int imageAtomicMax(image2D image, ivec2 P, int data) {}
int imageAtomicMax(iimage2D image, ivec2 P, int data) {}
int imageAtomicMax(uimage2D image, ivec2 P, int data) {}
int imageAtomicMax(image3D image, ivec3 P, int data) {}
int imageAtomicMax(iimage3D image, ivec3 P, int data) {}
int imageAtomicMax(uimage3D image, ivec3 P, int data) {}
int imageAtomicMax(imageCube image, ivec3 P, int data) {}
int imageAtomicMax(iimageCube image, ivec3 P, int data) {}
int imageAtomicMax(uimageCube image, ivec3 P, int data) {}
int imageAtomicMax(imageBuffer image, int P, int data) {}
int imageAtomicMax(iimageBuffer image, int P, int data) {}
int imageAtomicMax(uimageBuffer image, int P, int data) {}
int imageAtomicMax(image2DArray image, ivec3 P, int data) {}
int imageAtomicMax(iimage2DArray image, ivec3 P, int data) {}
int imageAtomicMax(uimage2DArray image, ivec3 P, int data) {}
int imageAtomicMax(imageCubeArray image, ivec3 P, int data) {}
int imageAtomicMax(iimageCubeArray image, ivec3 P, int data) {}
int imageAtomicMax(uimageCubeArray image, ivec3 P, int data) {}
int imageAtomicMax(image1D image, int P, int data) {}
int imageAtomicMax(iimage1D image, int P, int data) {}
int imageAtomicMax(uimage1D image, int P, int data) {}
int imageAtomicMax(image1DArray image, ivec2 P, int data) {}
int imageAtomicMax(iimage1DArray image, ivec2 P, int data) {}
int imageAtomicMax(uimage1DArray image, ivec2 P, int data) {}
int imageAtomicMax(image2DMS image, ivec2 P, int sample, int data) {}
int imageAtomicMax(iimage2DMS image, ivec2 P, int sample, int data) {}
int imageAtomicMax(uimage2DMS image, ivec2 P, int sample, int data) {}
int imageAtomicMax(image2DMSArray image, ivec3 P, int sample, int data) {}
int imageAtomicMax(iimage2DMSArray image, ivec3 P, int sample, int data) {}
int imageAtomicMax(uimage2DMSArray image, ivec3 P, int sample, int data) {}
uint imageAtomicAnd(image2D image, ivec2 P, uint data) {}
uint imageAtomicAnd(iimage2D image, ivec2 P, uint data) {}
uint imageAtomicAnd(uimage2D image, ivec2 P, uint data) {}
uint imageAtomicAnd(image3D image, ivec3 P, uint data) {}
uint imageAtomicAnd(iimage3D image, ivec3 P, uint data) {}
uint imageAtomicAnd(uimage3D image, ivec3 P, uint data) {}
uint imageAtomicAnd(imageCube image, ivec3 P, uint data) {}
uint imageAtomicAnd(iimageCube image, ivec3 P, uint data) {}
uint imageAtomicAnd(uimageCube image, ivec3 P, uint data) {}
uint imageAtomicAnd(imageBuffer image, int P, uint data) {}
uint imageAtomicAnd(iimageBuffer image, int P, uint data) {}
uint imageAtomicAnd(uimageBuffer image, int P, uint data) {}
uint imageAtomicAnd(image2DArray image, ivec3 P, uint data) {}
uint imageAtomicAnd(iimage2DArray image, ivec3 P, uint data) {}
uint imageAtomicAnd(uimage2DArray image, ivec3 P, uint data) {}
uint imageAtomicAnd(imageCubeArray image, ivec3 P, uint data) {}
uint imageAtomicAnd(iimageCubeArray image, ivec3 P, uint data) {}
uint imageAtomicAnd(uimageCubeArray image, ivec3 P, uint data) {}
uint imageAtomicAnd(image1D image, int P, uint data) {}
uint imageAtomicAnd(iimage1D image, int P, uint data) {}
uint imageAtomicAnd(uimage1D image, int P, uint data) {}
uint imageAtomicAnd(image1DArray image, ivec2 P, uint data) {}
uint imageAtomicAnd(iimage1DArray image, ivec2 P, uint data) {}
uint imageAtomicAnd(uimage1DArray image, ivec2 P, uint data) {}
uint imageAtomicAnd(image2DMS image, ivec2 P, int sample, uint data) {}
uint imageAtomicAnd(iimage2DMS image, ivec2 P, int sample, uint data) {}
uint imageAtomicAnd(uimage2DMS image, ivec2 P, int sample, uint data) {}
uint imageAtomicAnd(image2DMSArray image, ivec3 P, int sample, uint data) {}
uint imageAtomicAnd(iimage2DMSArray image, ivec3 P, int sample, uint data) {}
uint imageAtomicAnd(uimage2DMSArray image, ivec3 P, int sample, uint data) {}
int imageAtomicAnd(image2D image, ivec2 P, int data) {}
int imageAtomicAnd(iimage2D image, ivec2 P, int data) {}
int imageAtomicAnd(uimage2D image, ivec2 P, int data) {}
int imageAtomicAnd(image3D image, ivec3 P, int data) {}
int imageAtomicAnd(iimage3D image, ivec3 P, int data) {}
int imageAtomicAnd(uimage3D image, ivec3 P, int data) {}
int imageAtomicAnd(imageCube image, ivec3 P, int data) {}
int imageAtomicAnd(iimageCube image, ivec3 P, int data) {}
int imageAtomicAnd(uimageCube image, ivec3 P, int data) {}
int imageAtomicAnd(imageBuffer image, int P, int data) {}
int imageAtomicAnd(iimageBuffer image, int P, int data) {}
int imageAtomicAnd(uimageBuffer image, int P, int data) {}
int imageAtomicAnd(image2DArray image, ivec3 P, int data) {}
int imageAtomicAnd(iimage2DArray image, ivec3 P, int data) {}
int imageAtomicAnd(uimage2DArray image, ivec3 P, int data) {}
int imageAtomicAnd(imageCubeArray image, ivec3 P, int data) {}
int imageAtomicAnd(iimageCubeArray image, ivec3 P, int data) {}
int imageAtomicAnd(uimageCubeArray image, ivec3 P, int data) {}
int imageAtomicAnd(image1D image, int P, int data) {}
int imageAtomicAnd(iimage1D image, int P, int data) {}
int imageAtomicAnd(uimage1D image, int P, int data) {}
int imageAtomicAnd(image1DArray image, ivec2 P, int data) {}
int imageAtomicAnd(iimage1DArray image, ivec2 P, int data) {}
int imageAtomicAnd(uimage1DArray image, ivec2 P, int data) {}
int imageAtomicAnd(image2DMS image, ivec2 P, int sample, int data) {}
int imageAtomicAnd(iimage2DMS image, ivec2 P, int sample, int data) {}
int imageAtomicAnd(uimage2DMS image, ivec2 P, int sample, int data) {}
int imageAtomicAnd(image2DMSArray image, ivec3 P, int sample, int data) {}
int imageAtomicAnd(iimage2DMSArray image, ivec3 P, int sample, int data) {}
int imageAtomicAnd(uimage2DMSArray image, ivec3 P, int sample, int data) {}
uint imageAtomicOr(image2D image, ivec2 P, uint data) {}
uint imageAtomicOr(iimage2D image, ivec2 P, uint data) {}
uint imageAtomicOr(uimage2D image, ivec2 P, uint data) {}
uint imageAtomicOr(image3D image, ivec3 P, uint data) {}
uint imageAtomicOr(iimage3D image, ivec3 P, uint data) {}
uint imageAtomicOr(uimage3D image, ivec3 P, uint data) {}
uint imageAtomicOr(imageCube image, ivec3 P, uint data) {}
uint imageAtomicOr(iimageCube image, ivec3 P, uint data) {}
uint imageAtomicOr(uimageCube image, ivec3 P, uint data) {}
uint imageAtomicOr(imageBuffer image, int P, uint data) {}
uint imageAtomicOr(iimageBuffer image, int P, uint data) {}
uint imageAtomicOr(uimageBuffer image, int P, uint data) {}
uint imageAtomicOr(image2DArray image, ivec3 P, uint data) {}
uint imageAtomicOr(iimage2DArray image, ivec3 P, uint data) {}
uint imageAtomicOr(uimage2DArray image, ivec3 P, uint data) {}
uint imageAtomicOr(imageCubeArray image, ivec3 P, uint data) {}
uint imageAtomicOr(iimageCubeArray image, ivec3 P, uint data) {}
uint imageAtomicOr(uimageCubeArray image, ivec3 P, uint data) {}
uint imageAtomicOr(image1D image, int P, uint data) {}
uint imageAtomicOr(iimage1D image, int P, uint data) {}
uint imageAtomicOr(uimage1D image, int P, uint data) {}
uint imageAtomicOr(image1DArray image, ivec2 P, uint data) {}
uint imageAtomicOr(iimage1DArray image, ivec2 P, uint data) {}
uint imageAtomicOr(uimage1DArray image, ivec2 P, uint data) {}
uint imageAtomicOr(image2DMS image, ivec2 P, int sample, uint data) {}
uint imageAtomicOr(iimage2DMS image, ivec2 P, int sample, uint data) {}
uint imageAtomicOr(uimage2DMS image, ivec2 P, int sample, uint data) {}
uint imageAtomicOr(image2DMSArray image, ivec3 P, int sample, uint data) {}
uint imageAtomicOr(iimage2DMSArray image, ivec3 P, int sample, uint data) {}
uint imageAtomicOr(uimage2DMSArray image, ivec3 P, int sample, uint data) {}
int imageAtomicOr(image2D image, ivec2 P, int data) {}
int imageAtomicOr(iimage2D image, ivec2 P, int data) {}
int imageAtomicOr(uimage2D image, ivec2 P, int data) {}
int imageAtomicOr(image3D image, ivec3 P, int data) {}
int imageAtomicOr(iimage3D image, ivec3 P, int data) {}
int imageAtomicOr(uimage3D image, ivec3 P, int data) {}
int imageAtomicOr(imageCube image, ivec3 P, int data) {}
int imageAtomicOr(iimageCube image, ivec3 P, int data) {}
int imageAtomicOr(uimageCube image, ivec3 P, int data) {}
int imageAtomicOr(imageBuffer image, int P, int data) {}
int imageAtomicOr(iimageBuffer image, int P, int data) {}
int imageAtomicOr(uimageBuffer image, int P, int data) {}
int imageAtomicOr(image2DArray image, ivec3 P, int data) {}
int imageAtomicOr(iimage2DArray image, ivec3 P, int data) {}
int imageAtomicOr(uimage2DArray image, ivec3 P, int data) {}
int imageAtomicOr(imageCubeArray image, ivec3 P, int data) {}
int imageAtomicOr(iimageCubeArray image, ivec3 P, int data) {}
int imageAtomicOr(uimageCubeArray image, ivec3 P, int data) {}
int imageAtomicOr(image1D image, int P, int data) {}
int imageAtomicOr(iimage1D image, int P, int data) {}
int imageAtomicOr(uimage1D image, int P, int data) {}
int imageAtomicOr(image1DArray image, ivec2 P, int data) {}
int imageAtomicOr(iimage1DArray image, ivec2 P, int data) {}
int imageAtomicOr(uimage1DArray image, ivec2 P, int data) {}
int imageAtomicOr(image2DMS image, ivec2 P, int sample, int data) {}
int imageAtomicOr(iimage2DMS image, ivec2 P, int sample, int data) {}
int imageAtomicOr(uimage2DMS image, ivec2 P, int sample, int data) {}
int imageAtomicOr(image2DMSArray image, ivec3 P, int sample, int data) {}
int imageAtomicOr(iimage2DMSArray image, ivec3 P, int sample, int data) {}
int imageAtomicOr(uimage2DMSArray image, ivec3 P, int sample, int data) {}
uint imageAtomicXor(image2D image, ivec2 P, uint data) {}
uint imageAtomicXor(iimage2D image, ivec2 P, uint data) {}
uint imageAtomicXor(uimage2D image, ivec2 P, uint data) {}
uint imageAtomicXor(image3D image, ivec3 P, uint data) {}
uint imageAtomicXor(iimage3D image, ivec3 P, uint data) {}
uint imageAtomicXor(uimage3D image, ivec3 P, uint data) {}
uint imageAtomicXor(imageCube image, ivec3 P, uint data) {}
uint imageAtomicXor(iimageCube image, ivec3 P, uint data) {}
uint imageAtomicXor(uimageCube image, ivec3 P, uint data) {}
uint imageAtomicXor(imageBuffer image, int P, uint data) {}
uint imageAtomicXor(iimageBuffer image, int P, uint data) {}
uint imageAtomicXor(uimageBuffer image, int P, uint data) {}
uint imageAtomicXor(image2DArray image, ivec3 P, uint data) {}
uint imageAtomicXor(iimage2DArray image, ivec3 P, uint data) {}
uint imageAtomicXor(uimage2DArray image, ivec3 P, uint data) {}
uint imageAtomicXor(imageCubeArray image, ivec3 P, uint data) {}
uint imageAtomicXor(iimageCubeArray image, ivec3 P, uint data) {}
uint imageAtomicXor(uimageCubeArray image, ivec3 P, uint data) {}
uint imageAtomicXor(image1D image, int P, uint data) {}
uint imageAtomicXor(iimage1D image, int P, uint data) {}
uint imageAtomicXor(uimage1D image, int P, uint data) {}
uint imageAtomicXor(image1DArray image, ivec2 P, uint data) {}
uint imageAtomicXor(iimage1DArray image, ivec2 P, uint data) {}
uint imageAtomicXor(uimage1DArray image, ivec2 P, uint data) {}
uint imageAtomicXor(image2DMS image, ivec2 P, int sample, uint data) {}
uint imageAtomicXor(iimage2DMS image, ivec2 P, int sample, uint data) {}
uint imageAtomicXor(uimage2DMS image, ivec2 P, int sample, uint data) {}
uint imageAtomicXor(image2DMSArray image, ivec3 P, int sample, uint data) {}
uint imageAtomicXor(iimage2DMSArray image, ivec3 P, int sample, uint data) {}
uint imageAtomicXor(uimage2DMSArray image, ivec3 P, int sample, uint data) {}
int imageAtomicXor(image2D image, ivec2 P, int data) {}
int imageAtomicXor(iimage2D image, ivec2 P, int data) {}
int imageAtomicXor(uimage2D image, ivec2 P, int data) {}
int imageAtomicXor(image3D image, ivec3 P, int data) {}
int imageAtomicXor(iimage3D image, ivec3 P, int data) {}
int imageAtomicXor(uimage3D image, ivec3 P, int data) {}
int imageAtomicXor(imageCube image, ivec3 P, int data) {}
int imageAtomicXor(iimageCube image, ivec3 P, int data) {}
int imageAtomicXor(uimageCube image, ivec3 P, int data) {}
int imageAtomicXor(imageBuffer image, int P, int data) {}
int imageAtomicXor(iimageBuffer image, int P, int data) {}
int imageAtomicXor(uimageBuffer image, int P, int data) {}
int imageAtomicXor(image2DArray image, ivec3 P, int data) {}
int imageAtomicXor(iimage2DArray image, ivec3 P, int data) {}
int imageAtomicXor(uimage2DArray image, ivec3 P, int data) {}
int imageAtomicXor(imageCubeArray image, ivec3 P, int data) {}
int imageAtomicXor(iimageCubeArray image, ivec3 P, int data) {}
int imageAtomicXor(uimageCubeArray image, ivec3 P, int data) {}
int imageAtomicXor(image1D image, int P, int data) {}
int imageAtomicXor(iimage1D image, int P, int data) {}
int imageAtomicXor(uimage1D image, int P, int data) {}
int imageAtomicXor(image1DArray image, ivec2 P, int data) {}
int imageAtomicXor(iimage1DArray image, ivec2 P, int data) {}
int imageAtomicXor(uimage1DArray image, ivec2 P, int data) {}
int imageAtomicXor(image2DMS image, ivec2 P, int sample, int data) {}
int imageAtomicXor(iimage2DMS image, ivec2 P, int sample, int data) {}
int imageAtomicXor(uimage2DMS image, ivec2 P, int sample, int data) {}
int imageAtomicXor(image2DMSArray image, ivec3 P, int sample, int data) {}
int imageAtomicXor(iimage2DMSArray image, ivec3 P, int sample, int data) {}
int imageAtomicXor(uimage2DMSArray image, ivec3 P, int sample, int data) {}
uint imageAtomicExchange(image2D image, ivec2 P, uint data) {}
uint imageAtomicExchange(iimage2D image, ivec2 P, uint data) {}
uint imageAtomicExchange(uimage2D image, ivec2 P, uint data) {}
uint imageAtomicExchange(image3D image, ivec3 P, uint data) {}
uint imageAtomicExchange(iimage3D image, ivec3 P, uint data) {}
uint imageAtomicExchange(uimage3D image, ivec3 P, uint data) {}
uint imageAtomicExchange(imageCube image, ivec3 P, uint data) {}
uint imageAtomicExchange(iimageCube image, ivec3 P, uint data) {}
uint imageAtomicExchange(uimageCube image, ivec3 P, uint data) {}
uint imageAtomicExchange(imageBuffer image, int P, uint data) {}
uint imageAtomicExchange(iimageBuffer image, int P, uint data) {}
uint imageAtomicExchange(uimageBuffer image, int P, uint data) {}
uint imageAtomicExchange(image2DArray image, ivec3 P, uint data) {}
uint imageAtomicExchange(iimage2DArray image, ivec3 P, uint data) {}
uint imageAtomicExchange(uimage2DArray image, ivec3 P, uint data) {}
uint imageAtomicExchange(imageCubeArray image, ivec3 P, uint data) {}
uint imageAtomicExchange(iimageCubeArray image, ivec3 P, uint data) {}
uint imageAtomicExchange(uimageCubeArray image, ivec3 P, uint data) {}
uint imageAtomicExchange(image1D image, int P, uint data) {}
uint imageAtomicExchange(iimage1D image, int P, uint data) {}
uint imageAtomicExchange(uimage1D image, int P, uint data) {}
uint imageAtomicExchange(image1DArray image, ivec2 P, uint data) {}
uint imageAtomicExchange(iimage1DArray image, ivec2 P, uint data) {}
uint imageAtomicExchange(uimage1DArray image, ivec2 P, uint data) {}
uint imageAtomicExchange(image2DMS image, ivec2 P, int sample, uint data) {}
uint imageAtomicExchange(iimage2DMS image, ivec2 P, int sample, uint data) {}
uint imageAtomicExchange(uimage2DMS image, ivec2 P, int sample, uint data) {}
uint imageAtomicExchange(image2DMSArray image, ivec3 P, int sample, uint data) {}
uint imageAtomicExchange(iimage2DMSArray image, ivec3 P, int sample, uint data) {}
uint imageAtomicExchange(uimage2DMSArray image, ivec3 P, int sample, uint data) {}
int imageAtomicExchange(image2D image, ivec2 P, int data) {}
int imageAtomicExchange(iimage2D image, ivec2 P, int data) {}
int imageAtomicExchange(uimage2D image, ivec2 P, int data) {}
int imageAtomicExchange(image3D image, ivec3 P, int data) {}
int imageAtomicExchange(iimage3D image, ivec3 P, int data) {}
int imageAtomicExchange(uimage3D image, ivec3 P, int data) {}
int imageAtomicExchange(imageCube image, ivec3 P, int data) {}
int imageAtomicExchange(iimageCube image, ivec3 P, int data) {}
int imageAtomicExchange(uimageCube image, ivec3 P, int data) {}
int imageAtomicExchange(imageBuffer image, int P, int data) {}
int imageAtomicExchange(iimageBuffer image, int P, int data) {}
int imageAtomicExchange(uimageBuffer image, int P, int data) {}
int imageAtomicExchange(image2DArray image, ivec3 P, int data) {}
int imageAtomicExchange(iimage2DArray image, ivec3 P, int data) {}
int imageAtomicExchange(uimage2DArray image, ivec3 P, int data) {}
int imageAtomicExchange(imageCubeArray image, ivec3 P, int data) {}
int imageAtomicExchange(iimageCubeArray image, ivec3 P, int data) {}
int imageAtomicExchange(uimageCubeArray image, ivec3 P, int data) {}
int imageAtomicExchange(image1D image, int P, int data) {}
int imageAtomicExchange(iimage1D image, int P, int data) {}
int imageAtomicExchange(uimage1D image, int P, int data) {}
int imageAtomicExchange(image1DArray image, ivec2 P, int data) {}
int imageAtomicExchange(iimage1DArray image, ivec2 P, int data) {}
int imageAtomicExchange(uimage1DArray image, ivec2 P, int data) {}
int imageAtomicExchange(image2DMS image, ivec2 P, int sample, int data) {}
int imageAtomicExchange(iimage2DMS image, ivec2 P, int sample, int data) {}
int imageAtomicExchange(uimage2DMS image, ivec2 P, int sample, int data) {}
int imageAtomicExchange(image2DMSArray image, ivec3 P, int sample, int data) {}
int imageAtomicExchange(iimage2DMSArray image, ivec3 P, int sample, int data) {}
int imageAtomicExchange(uimage2DMSArray image, ivec3 P, int sample, int data) {}
float imageAtomicExchange(image2D image, ivec2 P, float data) {}
float imageAtomicExchange(iimage2D image, ivec2 P, float data) {}
float imageAtomicExchange(uimage2D image, ivec2 P, float data) {}
float imageAtomicExchange(image3D image, ivec3 P, float data) {}
float imageAtomicExchange(iimage3D image, ivec3 P, float data) {}
float imageAtomicExchange(uimage3D image, ivec3 P, float data) {}
float imageAtomicExchange(imageCube image, ivec3 P, float data) {}
float imageAtomicExchange(iimageCube image, ivec3 P, float data) {}
float imageAtomicExchange(uimageCube image, ivec3 P, float data) {}
float imageAtomicExchange(imageBuffer image, int P, float data) {}
float imageAtomicExchange(iimageBuffer image, int P, float data) {}
float imageAtomicExchange(uimageBuffer image, int P, float data) {}
float imageAtomicExchange(image2DArray image, ivec3 P, float data) {}
float imageAtomicExchange(iimage2DArray image, ivec3 P, float data) {}
float imageAtomicExchange(uimage2DArray image, ivec3 P, float data) {}
float imageAtomicExchange(imageCubeArray image, ivec3 P, float data) {}
float imageAtomicExchange(iimageCubeArray image, ivec3 P, float data) {}
float imageAtomicExchange(uimageCubeArray image, ivec3 P, float data) {}
float imageAtomicExchange(image1D image, int P, float data) {}
float imageAtomicExchange(iimage1D image, int P, float data) {}
float imageAtomicExchange(uimage1D image, int P, float data) {}
float imageAtomicExchange(image1DArray image, ivec2 P, float data) {}
float imageAtomicExchange(iimage1DArray image, ivec2 P, float data) {}
float imageAtomicExchange(uimage1DArray image, ivec2 P, float data) {}
float imageAtomicExchange(image2DMS image, ivec2 P, int sample, float data) {}
float imageAtomicExchange(iimage2DMS image, ivec2 P, int sample, float data) {}
float imageAtomicExchange(uimage2DMS image, ivec2 P, int sample, float data) {}
float imageAtomicExchange(image2DMSArray image, ivec3 P, int sample, float data) {}
float imageAtomicExchange(iimage2DMSArray image, ivec3 P, int sample, float data) {}
float imageAtomicExchange(uimage2DMSArray image, ivec3 P, int sample, float data) {}
uint imageAtomicCompSwap(image2D image, ivec2 P, uint compare, uint data) {}
uint imageAtomicCompSwap(iimage2D image, ivec2 P, uint compare, uint data) {}
uint imageAtomicCompSwap(uimage2D image, ivec2 P, uint compare, uint data) {}
uint imageAtomicCompSwap(image3D image, ivec3 P, uint compare, uint data) {}
uint imageAtomicCompSwap(iimage3D image, ivec3 P, uint compare, uint data) {}
uint imageAtomicCompSwap(uimage3D image, ivec3 P, uint compare, uint data) {}
uint imageAtomicCompSwap(imageCube image, ivec3 P, uint compare, uint data) {}
uint imageAtomicCompSwap(iimageCube image, ivec3 P, uint compare, uint data) {}
uint imageAtomicCompSwap(uimageCube image, ivec3 P, uint compare, uint data) {}
uint imageAtomicCompSwap(imageBuffer image, int P, uint compare, uint data) {}
uint imageAtomicCompSwap(iimageBuffer image, int P, uint compare, uint data) {}
uint imageAtomicCompSwap(uimageBuffer image, int P, uint compare, uint data) {}
uint imageAtomicCompSwap(image2DArray image, ivec3 P, uint compare, uint data) {}
uint imageAtomicCompSwap(iimage2DArray image, ivec3 P, uint compare, uint data) {}
uint imageAtomicCompSwap(uimage2DArray image, ivec3 P, uint compare, uint data) {}
uint imageAtomicCompSwap(imageCubeArray image, ivec3 P, uint compare, uint data) {}
uint imageAtomicCompSwap(iimageCubeArray image, ivec3 P, uint compare, uint data) {}
uint imageAtomicCompSwap(uimageCubeArray image, ivec3 P, uint compare, uint data) {}
uint imageAtomicCompSwap(image1D image, int P, uint compare, uint data) {}
uint imageAtomicCompSwap(iimage1D image, int P, uint compare, uint data) {}
uint imageAtomicCompSwap(uimage1D image, int P, uint compare, uint data) {}
uint imageAtomicCompSwap(image1DArray image, ivec2 P, uint compare, uint data) {}
uint imageAtomicCompSwap(iimage1DArray image, ivec2 P, uint compare, uint data) {}
uint imageAtomicCompSwap(uimage1DArray image, ivec2 P, uint compare, uint data) {}
uint imageAtomicCompSwap(image2DMS image, ivec2 P, int sample, uint compare, uint data) {}
uint imageAtomicCompSwap(iimage2DMS image, ivec2 P, int sample, uint compare, uint data) {}
uint imageAtomicCompSwap(uimage2DMS image, ivec2 P, int sample, uint compare, uint data) {}
uint imageAtomicCompSwap(image2DMSArray image, ivec3 P, int sample, uint compare, uint data) {}
uint imageAtomicCompSwap(iimage2DMSArray image, ivec3 P, int sample, uint compare, uint data) {}
uint imageAtomicCompSwap(uimage2DMSArray image, ivec3 P, int sample, uint compare, uint data) {}
int imageAtomicCompSwap(image2D image, ivec2 P, int compare, int data) {}
int imageAtomicCompSwap(iimage2D image, ivec2 P, int compare, int data) {}
int imageAtomicCompSwap(uimage2D image, ivec2 P, int compare, int data) {}
int imageAtomicCompSwap(image3D image, ivec3 P, int compare, int data) {}
int imageAtomicCompSwap(iimage3D image, ivec3 P, int compare, int data) {}
int imageAtomicCompSwap(uimage3D image, ivec3 P, int compare, int data) {}
int imageAtomicCompSwap(imageCube image, ivec3 P, int compare, int data) {}
int imageAtomicCompSwap(iimageCube image, ivec3 P, int compare, int data) {}
int imageAtomicCompSwap(uimageCube image, ivec3 P, int compare, int data) {}
int imageAtomicCompSwap(imageBuffer image, int P, int compare, int data) {}
int imageAtomicCompSwap(iimageBuffer image, int P, int compare, int data) {}
int imageAtomicCompSwap(uimageBuffer image, int P, int compare, int data) {}
int imageAtomicCompSwap(image2DArray image, ivec3 P, int compare, int data) {}
int imageAtomicCompSwap(iimage2DArray image, ivec3 P, int compare, int data) {}
int imageAtomicCompSwap(uimage2DArray image, ivec3 P, int compare, int data) {}
int imageAtomicCompSwap(imageCubeArray image, ivec3 P, int compare, int data) {}
int imageAtomicCompSwap(iimageCubeArray image, ivec3 P, int compare, int data) {}
int imageAtomicCompSwap(uimageCubeArray image, ivec3 P, int compare, int data) {}
int imageAtomicCompSwap(image1D image, int P, int compare, int data) {}
int imageAtomicCompSwap(iimage1D image, int P, int compare, int data) {}
int imageAtomicCompSwap(uimage1D image, int P, int compare, int data) {}
int imageAtomicCompSwap(image1DArray image, ivec2 P, int compare, int data) {}
int imageAtomicCompSwap(iimage1DArray image, ivec2 P, int compare, int data) {}
int imageAtomicCompSwap(uimage1DArray image, ivec2 P, int compare, int data) {}
int imageAtomicCompSwap(image2DMS image, ivec2 P, int sample, int compare, int data) {}
int imageAtomicCompSwap(iimage2DMS image, ivec2 P, int sample, int compare, int data) {}
int imageAtomicCompSwap(uimage2DMS image, ivec2 P, int sample, int compare, int data) {}
int imageAtomicCompSwap(image2DMSArray image, ivec3 P, int sample, int compare, int data) {}
int imageAtomicCompSwap(iimage2DMSArray image, ivec3 P, int sample, int compare, int data) {}
int imageAtomicCompSwap(uimage2DMSArray image, ivec3 P, int sample, int compare, int data) {}
float dFdx(float p) {}
vec2 dFdx(vec2 p) {}
vec3 dFdx(vec3 p) {}
vec4 dFdx(vec4 p) {}
float dFdy(float p) {}
vec2 dFdy(vec2 p) {}
vec3 dFdy(vec3 p) {}
vec4 dFdy(vec4 p) {}
float dFdxFine(float p) {}
vec2 dFdxFine(vec2 p) {}
vec3 dFdxFine(vec3 p) {}
vec4 dFdxFine(vec4 p) {}
float dFdyFine(float p) {}
vec2 dFdyFine(vec2 p) {}
vec3 dFdyFine(vec3 p) {}
vec4 dFdyFine(vec4 p) {}
float dFdxCoarse(float p) {}
vec2 dFdxCoarse(vec2 p) {}
vec3 dFdxCoarse(vec3 p) {}
vec4 dFdxCoarse(vec4 p) {}
float dFdyCoarse(float p) {}
vec2 dFdyCoarse(vec2 p) {}
vec3 dFdyCoarse(vec3 p) {}
vec4 dFdyCoarse(vec4 p) {}
float fwidth(float p) {}
vec2 fwidth(vec2 p) {}
vec3 fwidth(vec3 p) {}
vec4 fwidth(vec4 p) {}
float fwidthFine(float p) {}
vec2 fwidthFine(vec2 p) {}
vec3 fwidthFine(vec3 p) {}
vec4 fwidthFine(vec4 p) {}
float fwidthCoarse(float p) {}
vec2 fwidthCoarse(vec2 p) {}
vec3 fwidthCoarse(vec3 p) {}
vec4 fwidthCoarse(vec4 p) {}
float interpolateAtCentroid(float interpolant) {}
vec2 interpolateAtCentroid(vec2 interpolant) {}
vec3 interpolateAtCentroid(vec3 interpolant) {}
vec4 interpolateAtCentroid(vec4 interpolant) {}
float interpolateAtSample(float interpolant, int sample) {}
vec2 interpolateAtSample(vec2 interpolant, int sample) {}
vec3 interpolateAtSample(vec3 interpolant, int sample) {}
vec4 interpolateAtSample(vec4 interpolant, int sample) {}
float interpolateAtOffset(float interpolant, vec2 offset) {}
vec2 interpolateAtOffset(vec2 interpolant, vec2 offset) {}
vec3 interpolateAtOffset(vec3 interpolant, vec2 offset) {}
vec4 interpolateAtOffset(vec4 interpolant, vec2 offset) {}
void barrier() {}
void memoryBarrier() {}
void memoryBarrierAtomicCounter() {}
void memoryBarrierBuffer() {}
void memoryBarrierShared() {}
void memoryBarrierImage() {}
void groupMemoryBarrier() {}
vec4 subpassLoad(subpassInput subpass) {}
ivec4 subpassLoad(siubpassInput subpass) {}
uvec4 subpassLoad(usubpassInput subpass) {}
vec4 subpassLoad(subpassInputMS subpass, int sample) {}
ivec4 subpassLoad(isubpassInputMS subpass, int sample) {}
uvec4 subpassLoad(usubpassInputMS subpass, int sample) {}
bool anyInvocation(bool value) {}
bool allInvocations(bool value) {}
bool allInvocationsEqual(bool value) {})";

    [[nodiscard]] static anton::Array<Owning_Ptr<Variable_Declaration>> generate_builtin_variables(Context& ctx) {
        struct Builtin_Variable {
            anton::String_View name;
            Owning_Ptr<Type> type;
        };

        Source_Info const src_info{u8"<builtin>", 0, 0, 0};
        Builtin_Variable builtin_variables[] = {
            // Vertex Shader
            {"gl_VertexIndex", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_int, src_info)},
            {"gl_InstanceIndex", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_int, src_info)},
            {"gl_DrawID", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_int, src_info)},
            {"gl_BaseVertex", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_int, src_info)},
            {"gl_BaseInstance", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_int, src_info)},
            {"gl_Position", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_vec4, src_info)},
            {"gl_PointSize", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_float, src_info)},
            {"gl_ClipDistance", allocate_owning<Array_Type>(
                                    ctx.allocator, allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_float, src_info), nullptr, src_info)},
            {"gl_CullDistance", allocate_owning<Array_Type>(
                                    ctx.allocator, allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_float, src_info), nullptr, src_info)},
            // TODO: Add tessellation shader variables and geometry shader variables
            // Fragment Shader
            {"gl_FragCoord", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_vec4, src_info)},
            {"gl_FrontFacing", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_bool, src_info)},
            // gl_ClipDistance, gl_CullDistance already declared above in the vertex shader section
            {"gl_PointCoord", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_vec2, src_info)},
            {"gl_PrimitiveID", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_int, src_info)},
            {"gl_SampleID", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_int, src_info)},
            {"gl_SamplePosition", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_vec2, src_info)},
            {"gl_SampleMaskIn", allocate_owning<Array_Type>(ctx.allocator, allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_int, src_info),
                                                            nullptr, src_info)},
            {"gl_Layer", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_int, src_info)},
            {"gl_ViewportIndex", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_int, src_info)},
            {"gl_HelperInvocation", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_int, src_info)},
            {"gl_FragDepth", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_float, src_info)},
            {"gl_SampleMask", allocate_owning<Array_Type>(ctx.allocator, allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_int, src_info),
                                                          nullptr, src_info)},
            // Compute Shader
            {"gl_NumWorkGroups", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_uvec3, src_info)},
            {"gl_WorkgroupSize", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_uvec3, src_info)},
            {"gl_WorkGroupID", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_uvec3, src_info)},
            {"gl_LocalInvocationID", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_uvec3, src_info)},
            {"gl_GlobalInvocationID", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_uvec3, src_info)},
            {"gl_LocalInvocationIndex", allocate_owning<Builtin_Type>(ctx.allocator, Builtin_GLSL_Type::glsl_uint, src_info)}};

        // Populate storage.
        anton::Array<Owning_Ptr<Variable_Declaration>> storage;
        constexpr i64 variable_count = sizeof(builtin_variables) / sizeof(Builtin_Variable);
        for(i64 i = 0; i < variable_count; ++i) {
            Builtin_Variable& var = builtin_variables[i];
            Owning_Ptr decl = allocate_owning<Variable_Declaration>(
                ctx.allocator, ANTON_MOV(var.type), allocate_owning<Identifier>(ctx.allocator, anton::String(var.name), src_info), nullptr, src_info);
            storage.emplace_back(ANTON_MOV(decl));
        }
        return storage;
    }

    Builtin_Declarations get_builtin_declarations(Context& ctx) {
        anton::String functions_source{builtin_functions_declarations_source};
        anton::Expected<Declaration_List, Parse_Error> result = parse_builtin_functions(ctx.allocator, "<builtin>"_sv, functions_source);
        ANTON_ASSERT(result, "invalid builtin source code");
        ctx.source_registry.emplace("<builtin>"_sv, Source_Data{"<builtin>"_s, ANTON_MOV(functions_source)});

        anton::Flat_Hash_Map<anton::String_View, Owning_Ptr<Overloaded_Function_Declaration>> overloads_dictionary;
        for(auto& fn_ptr: result.value()) {
            Owning_Ptr<Function_Declaration> fn{downcast, ANTON_MOV(fn_ptr)};
            auto iter = overloads_dictionary.find(fn->identifier->value);
            if(iter == overloads_dictionary.end()) {
                // We use the source information of the first overload to be able to provide diagnostics without
                // having to complicate the code with special cases for handling Overloaded_Function_Declaration.
                Owning_Ptr<Overloaded_Function_Declaration> overloaded_fn =
                    allocate_owning<Overloaded_Function_Declaration>(ctx.allocator, fn->identifier->clone(ctx.allocator), fn->source_info);
                overloaded_fn->builtin = true;
                overloaded_fn->overloads.emplace_back(ANTON_MOV(fn));
                overloads_dictionary.emplace(overloaded_fn->identifier->value, ANTON_MOV(overloaded_fn));
            } else {
                iter->value->overloads.emplace_back(ANTON_MOV(fn));
            }
        }

        anton::Array<Owning_Ptr<Overloaded_Function_Declaration>> overloads;
        for(auto& [key, overloaded_fn]: overloads_dictionary) {
            overloads.emplace_back(ANTON_MOV(overloaded_fn));
        }

        anton::Array<Owning_Ptr<Variable_Declaration>> variables = generate_builtin_variables(ctx);
        return {ANTON_MOV(overloads), ANTON_MOV(variables)};
    }
} // namespace vush
