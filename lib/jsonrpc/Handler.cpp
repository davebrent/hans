#include "hans/jsonrpc/Handler.hpp"
#include "hans/jsonrpc/Message.hpp"
#include <cassert>
#include <chrono>

using namespace hans;

static int get_index(std::vector<hans_hash> hashes, hans_hash hash) {
  for (int i = 0; i < hashes.size(); ++i) {
    if (hashes.at(i) == hash) {
      return i;
    }
  }
  return -1;
}

void jsonrpc::Handler::add_method(hans_hash name, jsonrpc::Method* method) {
  m_lookup.push_back(name);
  m_method.push_back(method);
}

bool jsonrpc::Handler::has_method(hans_hash name) {
  return get_index(m_lookup, name) >= 0;
}

void jsonrpc::Handler::call_method(hans_hash name,
                                   const jsonrpc::Message& request,
                                   jsonrpc::Message& response) {
  int index = get_index(m_lookup, name);
  assert(index >= 0);
  m_method.at(index)->execute(request, response);
}

jsonrpc::LoggingHandler::LoggingHandler(jsonrpc::IHandler& next,
                                        common::Logger& logger)
    : m_next(next), m_logger(logger) {
}

void jsonrpc::LoggingHandler::add_method(hans_hash name,
                                         jsonrpc::Method* method) {
  m_next.add_method(name, method);
}

bool jsonrpc::LoggingHandler::has_method(hans_hash name) {
  return m_next.has_method(name);
}

void jsonrpc::LoggingHandler::call_method(hans_hash name,
                                          const jsonrpc::Message& request,
                                          jsonrpc::Message& response) {
  std::ostringstream pre;
  pre << "JSONRPC request=" << request.m_data;
  m_logger.log(common::Logger::DEBUG, pre.str().c_str());

  auto start = std::chrono::steady_clock::now();

  m_next.call_method(name, request, response);

  auto end = std::chrono::steady_clock::now();
  auto ms = std::chrono::duration<double, std::milli>(end - start).count();

  std::ostringstream post;
  post << "JSONRPC "
       << "response=" << response.m_data << " completed_in=" << ms << "ms";
  m_logger.log(common::Logger::DEBUG, post.str().c_str());
}
