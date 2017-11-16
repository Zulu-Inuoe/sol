# Overview
Common Lisp libraries & framework for UI applications.

# Developer Setup

## git repos

``` bash
;;Main project
git clone https://github.com/Zulu-Inuoe/sol.git
git clone https://github.com/Zulu-Inuoe/cl-sdl-gpu.git
git clone https://github.com/Zulu-Inuoe/cl-sdl2-gfx.git

;;On Windows
git clone https://github.com/Zulu-Inuoe/win32.git
```

## Dependencies

### Windows
NOTE: Need to install libffi
NOTE: Need to build SDL_gpu

TBD

### Linux

Install SDL2 dependencies

``` bash
sudo apt-get install libsdl2-2.0.0
sudo apt-get install libsdl2-image-2.0.0
sudo apt-get install libsdl2-mixer-2.0.0
sudo apt-get install libsdl2-ttf-2.0.0

sudo apt-get install libsdl2-gfx-1.0.0
```

For SDL_gpu

``` bash
sudo apt-get install cmake
sudo apt-get install libsdl2-dev
git clone https://github.com/grimfang4/sdl-gpu.git
cd sdl-gpu
cmake -G "Unix Makefiles"
make
sudo make install
```

On Ubuntu 16 to get the so where the system could find them I had to use

``` bash
cmake -G "Unix Makefiles" -DCMAKE_INSTALL_PREFIX=/usr`
```