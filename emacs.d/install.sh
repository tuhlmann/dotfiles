#!/bin/bash

if [[ ! -e ~/.cask ]]
then
    echo "Cloning Cask repo"
    git clone git@github.com:cask/cask.git ~/.cask
fi

if [[ $(grep "cask/bin" ~/.bashrc) == "" ]]
then
    echo "Adding \$HOME/.cask/bin to \$PATH in ~/.bashrc"
    echo '' >> ~/.bashrc
    echo "# Added by ~/.emacs.d/install.sh" >> ~/.bashrc
    echo "export PATH=\$HOME/.cask/bin:\$PATH" >> ~/.bashrc
fi

export PATH=$HOME/.cask/bin:$PATH

cd ~/.emacs.d
cask install

