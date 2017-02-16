#ifndef HANS_USERCONFIGCOMPILER_H_
#define HANS_USERCONFIGCOMPILER_H_

#include "hans/primitives.hpp"
#include "hans/user_config.hpp"

namespace hans {

bool compile_config(const user_data& input, EngineData& output);

} // hans

#endif // HANS_USERCONFIGCOMPILER_H_
