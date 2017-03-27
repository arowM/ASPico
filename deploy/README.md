# Deployment

This directory contains files for staging/production server setup/deployment.

## Deploy

Run following command on the parent directory (root of aspico repository).

```bash
./deploy/deploy.sh SERVER_HOST_NAME
```

where `SERVER_HOST_NAME` is the host name for the server you want to deploy to.
(i.e., The `ssh SERVER_HOST_NAME` successfully connected to the server.)

The deploy command makes it possible to realize no-down-time deployment with the help of [`keter`](https://hackage.haskell.org/package/keter).

## Set up server

In this section, I'll mention about set up steps for Ubuntu 16.04 server.

### Install dependent files

```bash
sudo apt-get -y install ufw postgresql
```

### Fire wall settings

If you planning to run aspico server on port `PORT`, open the port with [`ufw`](https://help.ubuntu.com/community/UFW).
Also, make sure to open port for ssh.

### PostgreSQL settings

Make sure to replace `YOUR_PASSWORD_HERE` with strong password.

```bash
sudo -u postgres -- psql --command "CREATE ROLE aspico NOSUPERUSER NOCREATEDB NOCREATEROLE INHERIT LOGIN ENCRYPTED PASSWORD 'YOUR_PASSWORD_HERE'"
sudo -u postgres -- createdb aspico
sudo -u postgres -- psql --command "GRANT ALL PRIVILEGES ON DATABASE aspico TO aspico"
```

### Add user for deployment

For security reason, add a non-sudoers user for deployment.

```bash
sudo adduser aspico
sudo -i -u aspico
mkdir -p .ssh/
# Paste your id_rsa.pub for ssh as this user
cat >> .ssh/authorized_keys
```

### Set up keter

First, install `keter` on your local machine and copy it to the server.

```bash
# on your local machine.
stack install keter
scp ~/.local/bin/keter ${SUDOERS_USER}@${SERVER_HOST}:
```

Second, after ssh to the server as a sudoers user, set up keter.

```bash
# on remote machine
sudo mkdir -p /opt/keter/bin
sudo cp ~/keter /opt/keter/bin
sudo mkdir -p /opt/aspico/etc
cat > /tmp/aspico-config.yaml <<EOF
# Directory containing incoming folder, where to store logs, etc. Relative to
# the config file directory.
root: ..

# Keter can listen on multiple ports for incoming connections. These ports can
# have HTTPS either enabled or disabled.
listeners:
    # HTTP
    - host: "*4" # Listen on all IPv4 hosts
      port: 8055 # Could be used to modify port
    # HTTPS
    # - host: "*4"
    #   #port: 443
    #   key: key.pem
    #   certificate: certificate.pem

# User to run applications as

# setuid: ubuntu

# Get the user's IP address from x-forwarded-for. Useful when sitting behind a
# load balancer like Amazon ELB.

# ip-from-header: true
EOF
sudo chown root:root /tmp/aspico-config.yaml
sudo mv /tmp/aspico-config.yaml /opt/aspico/etc
sudo mkdir -p /opt/aspico/incoming
sudo chown aspico /opt/aspico/incoming
```

### Start `keter` daemon

```bash
# on remote machine
# Make sure to replace `xxxx` with real password and api key.
cat > /tmp/aspico.service <<EOF
[Unit]
Description=Keter
After=network.service

[Service]
Type=simple
ExecStart = /opt/keter/bin/keter /opt/aspico/etc/aspico-config.yaml
Environment=ASPICO_DB_PASSWORD=xxxx
Restart = always

[Install]
WantedBy=multi-user.target
EOF

sudo chown root:root /tmp/aspico.service
sudo mv /tmp/aspico.service /etc/systemd/system/
sudo systemctl enable aspico
sudo systemctl start aspico
```
