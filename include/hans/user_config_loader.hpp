#ifndef HANS_USERCONFIGLOADER_H_
#define HANS_USERCONFIGLOADER_H_

#include "hans/user_config.hpp"

namespace hans {

bool load_config(const char* filepath, user_data& data);

} // hans

#endif // HANS_USERCONFIGLOADER_H_
