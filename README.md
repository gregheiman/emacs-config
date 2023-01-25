
<p align="center"><img src="https://upload.wikimedia.org/wikipedia/commons/thumb/0/08/EmacsIcon.svg/1024px-EmacsIcon.svg.png" width="23%"></img></p>
<h1 align="center">Emacs Configuration</h1>
My personal configuration for the Emacs text editor. My installation aims to focus solely on text editing tasks, although I do currently have some configuration for Gnus email reader as well.
My Emacs configuration is based heavily on Evil mode. This allows Emacs to emulate Vi/Vim functionality and keybindings, making for an easier and more pleasant text editing experience.
This configuration is tested primarily on Linux based systems meaning that full compatibility can not be gauranteed for other operating systems. However, the configuration uses very little
in the way of Linux only package and configuration so this configuration should be mostly cross platform compatible.

## Table of Contents
- [Installation](#installation)
- [Byte Compiling](#byte-compiling)

<a name="installation"></a>
### Installation
Installation for this configuration is very straight forward.

1. Install Emacs onto your system. The configuration is tested with the latest Emacs release, so that is the release that I recommend.

2. Clone this repository to your system.

3. Create an `~/.emacs.d` folder if necessary. (On Windows this folder is located in `~/AppData/Roaming/.emacs.d`)

4. Create symlinks for the following:
    ```
    init.el -> ~/.emacs.d/init.el
    early-init.el -> ~/.emacs.d/early-init.el
    /Custom/ -> ~/.emacs.d/Custom
    /include/ -> ~/.emacs.d/include
    ```
    
5. Start up Emacs.

On the intial startup Emacs should install all packages automatically. Once this is complete you can restart Emacs and get to work.

If you get an error about the Hydra package simply run the following command:
```
M-X package-install hydra
```
This will install the Hydra package. Simply restart Emacs and it should automatically install the rest of the packages.

<a name="byte-compiling"></a>
### Byte Compiling
If you would like to eek possibly a bit more performance out of Emacs you can byte compile the files inside of the `/Custom/` folder.
These files are not changed very often and they can improve performance very slightly. 

If you do make changes to one of these files you must redo the byte compiling for the file.
