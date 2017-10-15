# TeX Environment

## Installation

```bash
mkdir vendor

# You may need to change the exclusion patterns based on your system and arch
rsync -av --progress \
  --exclude '*.doc.tar.xz' --exclude '*.source.tar.xz' \
  --exclude '*-cygwin.tar.xz*' --exclude '*.win32.tar.xz' \
  --exclude '*-darwin.tar.xz' --exclude '*-darwinlegacy.tar.xz' \
  --exclude '*-solaris.tar.xz' --exclude '*-netbsd.tar.xz' --exclude '*-freebsd.tar.xz' \
  --exclude '*.armel-linux.tar.xz' --exclude '*.armhf-linux.tar.xz' \
  --exclude '*.i386-linux.tar.xz' --exclude '*.powerpc-linux.tar.xz' \
  rsync://mirrors.mi.ras.ru/CTAN/systems/texlive/tlnet/ ./vendor

cd vendor
chmod +x ./install-tl

./install-tl -portable -in-place
# During the installation:
# 1. Change the TEXDIR to <path to your home directory>/.texlive
# 2. Deselect "install macro/font doc tree" and "install macro/font source tree"
# 3. Proceed by entering I <ENTER>
```
