// #include "hans/audio/AudioBufferManager.hpp"
// #include <catch.hpp>

// using namespace hans;

// SCENARIO("allocating audio buffers", "[audio]") {
//   GIVEN("a request for audio buffers") {
//     audio::AudioBufferManager audio_buffer_manager(512);

//     WHEN("requesting multiple buffers") {
//       auto buffers = audio_buffer_manager.create(2, 10, 2);

//       THEN("it should allocate the buffer and return a pointer") {
//         for (int i = 0; i < 2; ++i) {
//           REQUIRE(buffers[i].channels == 2);
//           REQUIRE(buffers[i].samples_len == 10);
//         }
//       }

//       THEN("it should not corrupt memory") {
//         for (int b = 0; b < 2; ++b) {
//           for (int c = 0; c < buffers[b].channels; ++c) {
//             for (int i = 0; i < buffers[b].samples_len; ++i) {
//               buffers[b].samples[c][i] = 100;
//             }
//           }

//           REQUIRE(buffers[b].channels == 2);
//           REQUIRE(buffers[b].samples_len == 10);
//           REQUIRE(buffers[b].samples[0][0] == 100);
//           REQUIRE(buffers[b].samples[0][9] == 100);
//         }
//       }
//     }
//   }
// }
