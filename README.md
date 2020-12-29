# Dotfiles

Personal repository containing all configuration for local development. Uses Ansible for setup.


## Dependencies

The following are required for automated environment setup:

- [Ansible](https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html)
- [Homebrew](https://docs.brew.sh/Installation)


## Setup

Once repo is cloned and dependencies are installed:

```sh
ansible-playbook playbook.yaml -i hosts.yaml
```
