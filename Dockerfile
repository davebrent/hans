FROM debian:jessie

RUN apt-get clean && apt-get update && \
    apt-get install -y --no-install-recommends \
  build-essential \
  cmake \
  premake4 \
  ca-certificates \
  git \
  mercurial \
  valgrind \
  portaudio19-dev \
  libepoxy-dev \
  libglfw3-dev \
  libfreeimage-dev \
  libxrandr-dev \
  libxinerama-dev \
  libxi-dev \
  libxcursor-dev \
  librtmidi-dev \
  libglm-dev \
  libaubio-dev \
  libvpx-dev

RUN mkdir -p /app
WORKDIR /app

# Install catch
RUN cd /app && \
  git clone https://github.com/philsquared/Catch.git && \
  cp Catch/single_include/catch.hpp /usr/include/catch.hpp

# Install nanovg
RUN cd /app && \
  git clone -b hans https://github.com/davebrent/nanovg.git && \
  cd /app/nanovg && premake4 gmake && cd build && make nanovg && cd .. && \
  premake4 --prefix=/usr/local install

RUN cd /app && \
  git clone https://github.com/USCiLab/cereal.git --depth=1 && \
  mv cereal/include/cereal/ /usr/include/cereal/

RUN cd /app && \
  git clone https://github.com/skystrife/cpptoml.git --depth=1 && \
  mv cpptoml/include/cpptoml.h /usr/include/cpptoml.h

RUN cd /app && \
  hg clone https://davebrent@bitbucket.org/SpartanJ/efsw && \
  cd /app/efsw && premake4 gmake && cd make/linux && make config=release && \
  mv ../../lib/libefsw.so /usr/local/lib/libefsw.so && \
  mv ../../include/efsw/ /usr/include/efsw/

COPY . /app
RUN mkdir -p /app/build && cd /app/build && cmake .. && make install

CMD /bin/bash
