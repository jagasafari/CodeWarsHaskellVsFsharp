apt-get update
apt-get upgrade
dpkg-reconfigure tzdata
apt-get install -y curl
apt-get install -y vim
apt-get install -y git
apt-get install -y make
apt-get install -y xz-utils
echo 'deb http://ftp.debian.org/debian/ jessie-backports main' | tee /etc/apt/sources.list.d/backports.list
apt-get -t jessie-backports install ghc
mkdir /stackTmp
cd /stackTmp
# not tested curl command
curl https://github.com/commercialhaskell/stack/releases/download/v1.3.2/stack-1.3.2-linux-arm.tar.gz
tar xf stack-1.3.2-linux-arm.tar.gz
cp /stackTmp/stack-1.3.2-linux-arm/stack /usr/local/bin
cd /
export STACK_ROOT=/.stack
stack update -v
stack setup 7.10.3 -v
cp ~/setup-linux-arm7/config.yaml /.stack/global-project
apt-get install -y zlib1g-dev
stack install cabal-install-1.22.5.0 --resolver ghc-7.10.3 -v
# ensure in .bashrc exportimg cabal
export PATH=$PATH:/.stack/global-project/.stack-work/install/arm-linux/ghc-7.10.3/7.10.3/bin
cd /stackTmp
stack new p --resolver ghc-7.10.3 -v
