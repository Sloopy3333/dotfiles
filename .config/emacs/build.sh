./configure --with-json \
            --without-gconf \
            --without-gsettings \
            --enable-link-time-optimization \
            --with-native-compilation \
            --with-pgtk \
            # --with-x-toolkit=gtk3 \
            --without-xaw3d \
            --without-compress-install \
            --with-jpeg \
            --with-png \
            --with-rsvg \
            --with-tiff \
            --with-wide-int \
            --with-xft \
            --with-xml2 \
            --with-xpm \
            --with-sound=alsa \
            --with-modules \
            --without-mailutils \
            CFLAGS="-flto -O3 -mtune=native -march=native -fomit-frame-pointer" prefix=/usr/local
