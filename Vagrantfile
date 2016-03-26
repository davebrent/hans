# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure(2) do |config|
  config.vm.box = "boxcutter/ubuntu1510"

  config.vm.provision "shell", inline: <<-SHELL
    sudo apt-get update
    sudo apt-get install -y build-essential
    sudo apt-get install -y git
    sudo apt-get install -y cmake
    sudo apt-get install -y vim
    sudo apt-get install -y valgrind
    sudo apt-get install -y sqlite3
    sudo apt-get install -y portaudio19-dev
    sudo apt-get install -y libsqlite3-dev
    sudo apt-get install -y libepoxy-dev
    sudo apt-get install -y libzmq3-dev
    sudo apt-get install -y libglfw3-dev libxrandr-dev libxinerama-dev \
      libxi-dev libxcursor-dev
    sudo apt-get install -y libavcodec-dev libavdevice-dev libavfilter-dev \
      libavformat-dev libavresample-dev libswscale-dev libavutil-dev

    # Tool dependencies
    sudo apt-get install -y python-dev
    sudo apt-get install -y python-pip
    sudo apt-get install -y python-virtualenv

    # Module dependencies
    sudo apt-get install -y libglm-dev

    export LD_LIBRARY_PATH=/vagrant/build/lib
  SHELL

  config.vm.provider "virtualbox" do |vb|
    vb.name = "hans-vagrant"
    vb.gui = false
    vb.memory = "1024"
    vb.cpus = 1
  end
end
