# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
  export ZSH=/home/vagrant/.oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)
path=($path "$HOME/bin")
source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# disable START/STOP output control (<C-S>, <C-Q>)
stty -ixon



# 檢查 ruby 語法
# Check changed *.(rb|jbuilder) files, except for Deleted, Renamed and Copied ones.
# Arguments will be delegated to git-diff --name-only.
cop() {
  local extra_options='--display-cop-names --rails'
  if [[ $# -gt 0 ]]; then
    if git diff --name-only $@ -- **/*.(rb|jbuilder) > /dev/null; then
      git diff --name-only $@ -- **/*.(rb|jbuilder) | xargs rubocop `echo $extra_options`
    fi
  else
    local files="$(gits --porcelain -- **/*.(rb|jbuilder) | sed -e '/^\s\?[DRC] /d' -e 's/^.\{3\}//g')"
    if [[ -n "$files" ]]; then
      echo $files | xargs rubocop `echo $extra_options`
    else
      echo "Nothing to check. Write some *.(rb|jbuilder) to check.\nYou have 20 seconds to comply."
    fi
  fi
}


# 重啟 puma/unicorn（非 daemon 模式，用於 pry debug）
rpy() {
  if bundle show pry-remote > /dev/null 2>&1; then
    bundle exec pry-remote
  else
    rpu pry
  fi
}


# 重啟 puma/unicorn
#
# - rpu       → 啟動或重啟（如果已有 pid）
# - rpu kill  → 殺掉 process，不重啟
# - rpu xxx   → xxx 參數會被丟給 pumactl（不支援 unicorn）
rpu() {
  emulate -L zsh
  if [[ -d tmp ]]; then
    local action=$1
    local pid
    local animal

    if [[ -f config/puma.rb ]]; then
      animal='puma'
    elif [[ -f config/unicorn.rb ]]; then
      animal='unicorn'
    else
      echo "No puma/unicorn directory, aborted."
      return 1
    fi

    if [[ -r tmp/pids/$animal.pid && -n $(ps h -p `cat tmp/pids/$animal.pid` | tr -d ' ') ]]; then
      pid=`cat tmp/pids/$animal.pid`
    fi

    if [[ -n $action ]]; then
      case "$action" in
        pry)
          if [[ -n $pid ]]; then
            kill -9 $pid && echo "Process killed ($pid)."
          fi
          rserver_restart $animal
          ;;
        kill)
          if [[ -n $pid ]]; then
            kill -9 $pid && echo "Process killed ($pid)."
          else
            echo "No process found."
          fi
          ;;
        *)
          if [[ -n $pid ]]; then
            # TODO: control unicorn
            pumactl -p $pid $action
          else
            echo 'ERROR: "No running PID (tmp/pids/puma.pid).'
          fi
      esac
    else
      if [[ -n $pid ]]; then
        # Alternatives:
        # pumactl -p $pid restart
        # kill -USR2 $pid && echo "Process killed ($pid)."

        # kill -9 (SIGKILL) for force kill
        kill -9 $pid && echo "Process killed ($pid)."
        rserver_restart $animal $([[ "$animal" == 'puma' ]] && echo '-d' || echo '-D')
      else
        rserver_restart $animal $([[ "$animal" == 'puma' ]] && echo '-d' || echo '-D')
      fi
    fi
  else
    echo 'ERROR: "tmp" directory not found.'
  fi
}


# 啟動／停止 sidekiq
rsidekiq() {
  emulate -L zsh
  if [[ -d tmp ]]; then
    if [[ -r tmp/pids/sidekiq.pid && -n $(ps h -p `cat tmp/pids/sidekiq.pid` | tr -d ' ') ]]; then
      case "$1" in
        restart)
          bundle exec sidekiqctl restart tmp/pids/sidekiq.pid
          ;;
        *)
          bundle exec sidekiqctl stop tmp/pids/sidekiq.pid
      esac
    else
      echo "Start sidekiq process..."
      nohup bundle exec sidekiq  > ~/.nohup/sidekiq.out 2>&1&
      disown %nohup
    fi
  else
    echo 'ERROR: "tmp" directory not found.'
  fi
}


# 啟動／停止 mailcatcher
rmailcatcher() {
  local pid=$(ps --no-headers -C mailcatcher -o pid,args | command grep '/bin/mailcatcher --http-ip' | sed 's/^ //' | cut -d' ' -f 1)
  if [[ -n $pid ]]; then
    kill $pid && echo "MailCatcher process $pid killed."
  else
    echo "Start MailCatcher process..."
    nohup mailcatcher --http-ip 0.0.0.0 > ~/.nohup/mailcatcher.out 2>&1&
    disown %nohup
  fi
}


# 這是 rpu 會用到的 helper function
rserver_restart() {
  local app=${$(pwd):t}
  case "$1" in
    puma)
      shift
      RAILS_RELATIVE_URL_ROOT=/$app bundle exec puma -C config/puma.rb config.ru $*
      ;;
    unicorn)
      shift
      RAILS_RELATIVE_URL_ROOT=/$app bundle exec unicorn -c config/unicorn.rb $* && echo 'unicorn running'
      ;;
    *)
      echo 'invalid argument'
  esac
}



# 常用 alias
alias n='cd nerv'
alias ll='ls -al'
alias rc='rails console development'

alias rm='rm -i'
alias be='bundle exec'
alias dump_db='/vagrant/var/scripts/dump_db.zsh'
alias rlog='tail -f log/development.log'




# chruby
# 1) 基本的 source chruby function
# 2) 切換目錄時，自動依 .ruby-version 切換 ruby
if [ -e /etc/profile.d/chruby.sh ]; then
  source /etc/profile.d/chruby.sh
elif [ -e /usr/local/opt/chruby/share/chruby/chruby.sh ]; then
  source /usr/local/opt/chruby/share/chruby/chruby.sh
elif [ -e /usr/local/share/chruby/chruby.sh ]; then
  source /usr/local/share/chruby/chruby.sh
  test -e /usr/local/share/chruby/auto.sh && source /usr/local/share/chruby/auto.sh
fi

pairg() { ssh -t $1 ssh -p $2 -t vagrant@localhost 'tmux attach' }
pairh() { ssh -R $2\:localhost:$2 -t $1 'htop' }
