# Disable VSCode Press and Hold
defaults write com.microsoft.VSCode ApplePressAndHoldEnabled -bool false

ln -s $PWD/zshrc ~/.zshrc
ln -s $PWD/karabiner ~/.config/karabiner

mkdir -p ~/.vim
cp -r $PWD/colors ~/.vim
ln -s $PWD/vimrc ~/.vimrc

