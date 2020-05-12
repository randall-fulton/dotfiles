CONFIG_DIR=$HOME/.config
DIR=$(dirname "$(readlink -f $0)")

error() {
	echo "FAIL: $1. File already exists."
}

error_exit() {
	error $1
	exit 1
}

skip() {
	echo "SKIP: $1. Already exists"
}

SRC=$DIR/nvim
DEST=$CONFIG_DIR/nvim
MSG="Create NeoVim symlink"

for f in $SRC/*; do
	SUB_SRC=$SRC/$f
	SUB_DEST=$DEST/$f
	if [ -L "$SUB_DEST" ]; then
		skip "$MSG"
	elif [ -f "$SUB_DEST" ]; then
		error "$MSG"
	else
		ln -s $SUB_SRC $SUB_DEST
	fi
done
