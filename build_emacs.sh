# sudo dnf install texinfo gtk3-devel glib2-devel webkit2gtk4.1-devel libgccjit-devel gnutls-devel libtree-sitter-devel
git clone --depth=1 https://git.savannah.gnu.org/git/emacs.git
cd emacs
./autogen.sh
./configure \
	--with-modules \
	--without-libotf \
	--without-gconf \
	--enable-link-time-optimization=yes \
	--with-xinput2 \
	--with-native-compilation=aot \
	--without-gsettings \
	--with-pgtk --without-xaw3d \
	--with-sound=alsa \
	--with-xwidgets \
	--with-tree-sitter \
	--with-mailutils \
	--without-compress-install \
	--prefix $HOME/.local/emacs 
make -j install
