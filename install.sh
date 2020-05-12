CONFIG_DIR=$HOME/.config
DIR=$(dirname "$(readlink -f $0)")

# copy <step> <src> <dest>
copy() {
	MSG="$1: $3"
	if [ -L "$3" ]; then
		skip "$MSG"
	elif [ -f "$3" ]; then
		error "$MSG"
	else
		ln -s $2 $3
	fi
}

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
for f in $SRC/*; do
	SUB_DEST="$DEST/$(basename $f)"
	copy "Create nvim symlinks" $f $SUB_DEST
done

copy "Create .zshrc" $DIR/.zshrc $HOME/.zshrc
