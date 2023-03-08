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
	--without-compress-install
