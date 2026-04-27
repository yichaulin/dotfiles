clear

# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH
export PATH=/opt/homebrew/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

ZSH_THEME="robbyrussell"



# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git asdf colorize)
source $ZSH/oh-my-zsh.sh



# You may need to manually set your language environment
# export LANG=en_US.UTF-8


# 常用 alias
alias ll='ls -al'
alias rm='rm -i'
alias dc='docker compose'
alias nl='nslookup'
alias be='bundle exec'
alias clear='clear && printf "\n%.0s" {1..$LINES}'

alias cc='docker run --rm -it \
  -v $(pwd):/app -v /app/.claude \
  -v ~/.claude/settings.json:/root/.claude/settings.json \
  -e GOPROXY=https://nexus.skyunion.net/repository/go-group,https://proxy.golang.org,direct \
  -e GONOSUMDB="git.skyunion.net/*" claude-code-dev-sandbox:latest'

# 讓游標從下方開始
printf "\n%.0s" {1..$LINES}
