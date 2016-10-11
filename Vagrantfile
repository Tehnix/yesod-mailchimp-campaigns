# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure('2') do |config|
  # Use Ubuntu 14.04 64-bit
  config.vm.box = 'ubuntu/trusty64'
  config.vm.box_check_update = false

  # Forward the SSH agent so SCP works with `yesod keter`
  config.ssh.forward_agent = true

  # Enable this to forward the yesod-devel port
  # config.vm.network "forwarded_port", guest: 3000, host: 3000
  # config.vm.network "public_network"

  # Provider-specific configuration
  config.vm.provider 'virtualbox' do |vb|
    vb.name = 'yesod-vm'
    # Customize the amount of memory and CPU cores on the VM
    vb.memory = '5120'
    vb.cpus = 3
    # Limit the VM to use a max of 50% of the host machine CPU
    # v.customize ["modifyvm", :id, "--cpuexecutioncap", "50"]
  end

  # Compile and upload the app using `yesod keter`
  config.push.define 'local-exec' do |push|
    push.inline = <<-SCRIPT
      vagrant plugin list | grep "vagrant-scp" && echo "Vagrant SCP already installed" || vagrant plugin install vagrant-scp
      vagrant scp ~/.ssh/campaigns.pem default:.ssh
      vagrant scp ~/.ssh/config default:.ssh
      vagrant ssh -c "cd /vagrant && stack exec -- yesod keter"
    SCRIPT
  end

  # Provision the VM by installing PostgreSQL and zlib header and development
  # files, along with setting up the stack environment for the app
  config.vm.provision 'shell', privileged: false, inline: <<-SHELL
    sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442
    echo "deb http://download.fpcomplete.com/ubuntu `lsb_release -sc` main"|sudo tee /etc/apt/sources.list.d/fpco.list

    sudo apt-get update
    sudo apt-get -y install libpq-dev zlib1g-dev stack

    cd /vagrant
    echo ">>> Running stack clean"
    stack clean
    echo ">>> Running stack update"
    stack update
    echo ">>> Running stack setup"
    stack setup
    echo ">>> Running stack build yesod-bin cabal-install"
    stack build yesod-bin cabal-install
    echo ">>> Running stack build"
    stack build
  SHELL
end
