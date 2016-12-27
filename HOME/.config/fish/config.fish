set -gx PYENV_ROOT $HOME/.pyenv
set -gx PATH $PYENV_ROOT/shims $PATH
set -gx PATH $PYENV_ROOT/bin $PATH
set -gx PATH /usr/local/bin $PATH


eval (python -m virtualfish)
set -gx VIRTUALFISH_HOME ~/pyvenvs


set -gx PATH $HOME/bin $PATH
