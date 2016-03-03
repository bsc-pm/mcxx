#!/bin/bash

die()
{
    echo "$*"
    exit 1
}

echo "Figuring out top level source tree..."

BASE_DIR=$(realpath "$0")
BASE_DIR=$(dirname "$BASE_DIR")
BASE_DIR="$BASE_DIR"/..
BASE_DIR=$(realpath "$BASE_DIR")

if [ ! -d "$BASE_DIR" ];
then
    die "Could not figure out base dir"
fi

echo $BASE_DIR
echo

# ------------------------------------------
# - hooks ----------------------------------
# ------------------------------------------
echo " - Setting up git hooks..."

GIT_DIR="$BASE_DIR"/.git

if [ ! -d $GIT_DIR ];
then
    die "git directory not found in $GIT_DIR"
fi

HOOKS_DIR="$GIT_DIR"/hooks

if [ ! -d "$HOOKS_DIR" ];
then
    die "git hooks directory not found in $HOOKS_DIR"
fi

if [ -e "$HOOKS_DIR/pre-commit" ];
then
    echo "$HOOKS_DIR/pre-commit already found, keeping it"
else
    ln -sv "$BASE_DIR/scripts/pre-commit-hook" "$HOOKS_DIR/pre-commit"
fi

echo

# ------------------------------------------
# - clang-format ---------------------------
# ------------------------------------------

echo " - Setting up $BASE_DIR/.clang-format ..."

if [ -e "$BASE_DIR/.clang-format" ];
then
    echo "$BASE_DIR/.clang-format already found, keeping it"
else
    cp -v "$BASE_DIR/scripts/clang-format" "$BASE_DIR/.clang-format"
fi

echo

# ------------------------------------------
echo "Setup finished"
