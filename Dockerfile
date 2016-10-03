FROM debian:jessie

RUN apt-get clean && apt-get update && \
    apt-get install -y --no-install-recommends \
  build-essential \
  cmake \
  premake4 \
  ca-certificates \
  git \
  valgrind \
  portaudio19-dev \
  libepoxy-dev \
  libglfw3-dev \
  libxrandr-dev \
  libxinerama-dev \
  libxi-dev \
  libxcursor-dev \
  libavcodec-dev \
  libavdevice-dev \
  libavfilter-dev \
  libavformat-dev \
  libavresample-dev \
  libswscale-dev \
  libavutil-dev \
  guile-2.0 \
  guile-2.0-dev \
  librtmidi-dev \
  libglm-dev \
  libaubio-dev

RUN mkdir -p /app
WORKDIR /app

# Install catch
RUN cd /app && \
  git clone https://github.com/philsquared/Catch.git && \
  cp Catch/single_include/catch.hpp /usr/include/catch.hpp

# Install nanovg (as a shared library)
RUN cd /app && git clone https://github.com/memononen/nanovg.git
RUN cd /app/nanovg && \
  sed -i -- 's/StaticLib/SharedLib/g' *premake4* && \
  premake4 gmake && \
  cd build/ && \
  make nanovg && \
  cp libnanovg.so /usr/lib
RUN mkdir -p /usr/local/lib/pkgconfig
RUN echo "libdir=/app/nanovg/build\n" \
         "includedir=/app/nanovg/src\n" \
         "Name: libnanovg\n" \
         "Description: Libnanovg - vector drawing library\n" \
         "Version: 1.0\n" \
         "Libs: -L/app/nanovg/build -lnanovg\n" \
         "Cflags: -I/app/nanovg/src" >> /usr/local/lib/pkgconfig/nanovg.pc

COPY . /app

# Install main libraries
RUN mkdir -p /app/build
RUN cd /app/build && cmake \
  -DCMAKE_BUILD_TYPE=Debug \
  -DCMAKE_INSTALL_PREFIX:PATH=/usr \
  .. && make install

# Install guile modules
RUN cd /usr/share/guile/2.0 && ln -s /app/scm/ hans
RUN echo "(use-modules (ice-9 readline)) (activate-readline)" > ~/.guile

# Build examples
RUN guile examples/programs.scm

CMD /bin/bash
