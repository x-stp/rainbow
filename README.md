# rainbow

text colorizer

## install

```
cabal build
```

## usage

```
./rainbow
./rainbow "your text"
```

## hp-ux

```
swinstall -s /depot/ghc-8.10.7.depot ghc
swlist -l product | grep -i haskell
/usr/local/bin/cabal v1-build --ghc-option=-optl-Wl,+s
chatr +s enable ./dist/build/rainbow/rainbow
./dist/build/rainbow/rainbow
```

## sunos/solaris

```
# solaris 2.1-2.5 (sparc)
pkgadd -d /cdrom/Solaris_2.5/Product SUNWgcc
/opt/SUNWspro/bin/cc -xarch=v8 -o bootstrap bootstrap.c
LD_LIBRARY_PATH=/usr/openwin/lib:/usr/dt/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH
./bootstrap && ./configure --with-gcc=/opt/gnu/bin/gcc
gmake LDFLAGS="-R/usr/local/lib -L/usr/local/lib"

# if on ultra 1/2 with creator3d
/usr/platform/sun4u/sbin/prtdiag | grep -i creator
ffbconfig -res 1152x900x76
```

## idrac5

```
# tested on poweredge 2950 idrac5 v1.33
ssh root@<idrac-ip>
/admin1-> racadm getsysinfo
/admin1-> racadm racdump
cd /tmp
tftp -i <your-ip> -c get rainbow
chmod +x rainbow
./rainbow "dell remote access"
# note: ansi colors may not render correctly in idrac5 console
# use serial redirect for better results
```

## xterm

```
# classic X11R6 xterm with proper 256 color support
xterm -geometry 80x45 -fn 9x15 -bg black -fg white +sb -tn xterm-256color
export TERM=xterm-256color

# if on old X11R5 or earlier
xrdb -merge <<EOF
XTerm*decTerminalID: 220
XTerm*numColorRegisters: 256
XTerm*color0: rgb:00/00/00
XTerm*color1: rgb:cc/00/00
! ... define all 256 colors if you're patient
EOF

# test your ancient cde/motif setup
/usr/dt/bin/dtterm -geometry 80x45 -ms red -cr green
xwininfo -root | grep -i depth

# on sgi irix with 4dwm
/usr/sbin/xsetmon -n 72hz
/usr/bin/X11/xterm -geometry 80x45 -fn "-sgi-screen-medium-r-normal--15-150-72-72-m-80-iso8859-1"
```

## license

XFree86 License 1.1