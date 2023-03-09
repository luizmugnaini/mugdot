import subprocess as sp


compulsory_packages = {
    "amd": ["amd-ucode", "xf86-video-amdgpu"],
    "user utils": [
        "git",
        "sudo",
        "redshift",
        "feh",
        "man-db",
        "man-pages",
        "pacman-contrib",
        "dunst",
        "stow",
        "xclip",
    ],
    "xorg": [
        "xorg-server",
        "xorg-xsetroot",
        "xorg-xrandr",
        "xorg-xmodmap",
        "xorg-xset",
        "xorg-setxkbmap",
        "xorg-utils",
        "xorg-xinit",
    ],
    "graphics": ["mesa"],
    "display manager": ["lightdm", "lightdm-gtk-greeter"],
    "audio": [
        "pipewire",
        "pipewire-audio",
        "pipewire-alsa",
        "pipewire-pulse",
        "rtkit",
        "pipewire-jack",
        "pipewire-docs",
        "pamixer",
        "wireplumber",
    ],
    "networking": ["networkmanager", "dhclient", "iwd", "wireless_tools"],
    "bluetooth": ["bluez", "bluez-utils", "bluez-libs"],
    "compositor": ["picom"],
    "fonts": [
        "nerd-fonts-complete",
        "ttf-apple-emoji",
        "adobe-source-code-pro-fonts",
        "ttf-fira-code",
    ],
    "package utils": ["automake", "cmake", "make"],
    "rust": ["rustup"],
}

optional_packages = {
    "emacs stuff": ["emacs", "cask", "emacs-pdf-tools", "emacs-tablist-git"],
    "shell": ["zsh", "zsh-sintax-highlighting"],
    "terminal stuff": ["neovim", "tmux", "alacritty"],
    "dwm stuff": ["slock", "xidlehook", "xautolock", "rofi"],
    "web": ["firefox"],
    "spotify": ["ncspot"],
    "rust": ["rust-analyzer"],
    "rustup components": ["clippy", "rustfmt"],
    "python": [
        "python-poetry",
        "pyright",
        "python-pip",
        "python-pydantic",
        "ipython",
        "jupyterlab",
        "tk",
    ],
    "programming tools": ["tree-sitter"],
    "personal website dev": ["zola"],
    "sage math": ["sagemath"],
    "databases": ["mariadb"],
    "latex": ["texlive-most", "biber", "latex-mk", "texlive-langjapanese"],
    "documents": [
        "zathura",
        "zathura-pdf-mupdf",
        "zathura-djvu",
        "djvulibre",
        "ghostscript",
        "poppler",
    ],
    "utilities": [
        "ripgrep",
        "zoxide",
        "exa",
        "bat",
        "fd",
        "starship",
        "fzf",
        "nnn-nerd",
        "python-pywal",
        "7-zip",
        "aspell",
        "ffmpeg",
        "github-cli",
        "imagemagick",
        "scrot",
        "ncdu",
        "unzip",
        "wget",
        "which",
    ],
    "onedrive": ["onedrive-abraunegg-git"],
}

PARU_INSTALL = ["paru", "-S"]


def header(action: str):
    action = " " + action + " "
    print(f"\n{action:-^80}\n")


def subaction(subact: str):
    indicator = "\033[1;32m::\033[0m"
    print(f"\n{indicator} {subact}\n")


def setup_network():
    header("Network setup")

    config_path = "/etc/NetworkManager/conf.d"
    subaction("setup dhcp")
    content = sp.Popen(["printf", "[main]\ndhcp=dhclient"], stdout=sp.PIPE)
    sp.run(
        ["sudo", "tee", "dhcp-client.conf"],
        stdin=content.stdout,
        cwd=config_path,
        stdout=sp.DEVNULL,
    )

    subaction("setup wifi backend")
    content = sp.Popen(["printf", "[device]\nwifi.backend=iwd"], stdout=sp.PIPE)
    sp.run(
        ["sudo", "tee", "wifi_backend.conf"],
        stdin=content.stdout,
        cwd=config_path,
        stdout=sp.DEVNULL,
    )

    sp.run(["sudo", "systemctl", "enable", "{NetworkManager,iwd}"])
    sp.run(["sudo", "systemctl", "start", "{NetworkManager,iwd}"])


def setup_paru():
    header("Paru AUR helper setup")

    subaction("installing paru requirements")
    sp.run(["sudo", "pacman", "-S", "--needed", "base-devel"])

    subaction("cloning paru")
    sp.run(["git", "clone", "https://aur.archlinux.org/paru.git"], cwd="/home/mug")

    subaction("making paru")
    sp.run(["makepkg", "-si"], cwd="/home/mug/paru")
    sp.run(["rm", "-rf", "paru"], cwd="/home/mug")

    subaction("Updating mirrors")
    # Uncomment multilib from /etc/pacman.conf
    # NOTE: uses https://unix.stackexchange.com/a/26289
    # TODO: what if perl is not installable at this point
    sp.run(["sudo", "pacman", "-S", "perl"])
    sp.run(
        [
            "perl",
            "-0777",
            "-pi",
            "-e",
            "s/#\\[multilib\\]\n#Include = \\/etc\\/pacman.d\\/mirrorlist/\\[multilib\\]\nInclude = \\/etc\\/pacman.d\\/mirrorlist/g",
            "/etc/pacman.conf",
        ]
    )
    sp.run(["paru", "-Syy"])


def setup_grub():
    header("Grub setup")
    subaction("installing grub via pacman")
    sp.run(["pacman", "-S", "grub", "efibootmgr"])
    subaction("installing grub via 'grub-install'")
    sp.run(
        [
            "grub-install",
            "--target=x86_64-efi",
            "--efi-directory=/boot/efi",
            "--bootloader-id=arch_grub",
            "--recheck",
        ]
    )
    subaction("Making grub configuration with 'grub-mkconfig'")
    sp.run(["grub-mkconfig", "-o", "/boot/grub/grub.cfg"])


def initial_setup():
    header("Creating a user named 'mug'")
    sp.run(["useradd", "m", "-g", "users", "-G", "wheel", "mug"])
    subaction("Creating a password for the user 'mug'")
    sp.run(["passwd", "mug"])

    header("Installing networking packages")
    sp.run(["sudo", "pacman", "-S"] + compulsory_packages["networking"])
    setup_network()


def post_initial_setup():
    header("Post-initial setup")
    sp.run(
        ["sudo", "pacman", "-Sy"]
        + compulsory_packages["xorg"]
        + compulsory_packages["amd"]
        + compulsory_packages["audio"]
    )

    # Audio handling
    sp.run(["sudo", "mkdir", "/etc/pipewire"])
    sp.run(["sudo", "cp", "/usr/share/pipewire/pipewire*", "/etc/pipewire/"])
    sp.run(["systemctl", "--user", "enable", "pipewire{,-pulse}"])
    sp.run(["systemctl", "--user", "start", "pipewire{,-pulse}"])

    # Install and setup paru
    sp.run(["sudo", "pacman", "-S"] + compulsory_packages["rust"])
    sp.run(["rustup", "default", "nightly"])
    setup_paru()

    sp.run(
        ["paru", "-S"]
        + compulsory_packages["bluetooth"]
        + compulsory_packages["display manager"]
        + compulsory_packages["compositor"]
        + compulsory_packages["graphics"]
        + compulsory_packages["fonts"]
        + compulsory_packages["package utils"]
        + compulsory_packages["user utils"]
        + compulsory_packages["rust"]
    )

    # Display manager
    sp.run(["sudo", "systemctl", "enable", "lightdm"])
    sp.run(["sudo", "systemctl", "start", "lightdm"])

    # Bluetooth
    sp.run(["sudo", "systemctl", "enable", "bluetooth"])
    sp.run(["sudo", "systemctl", "start", "bluetooth"])

    sp.run(["stow", "x"])


def setup_dwm():
    header("Setup dwm window manager")
    sp.run(PARU_INSTALL + optional_packages["dwm stuff"])

    common_path = "/home/mug/.mugdot/dwm/.config/"
    sp.run(["sudo", "make", "clean", "install"], cwd=common_path + "dwm-6.2")

    sp.run("make", cwd=common_path + "dwmblocks")
    sp.run(["sudo", "make", "install"], cwd=common_path + "dwmblocks")
    sp.run(["stow", "dunst", "rofi"])


def setup_shell():
    subaction("installing zhs stuff")
    sp.run(PARU_INSTALL + optional_packages["shell"])

    subaction("make zsh default shell")
    sp.run(["chsh", "-s", "/bin/zsh"])
    sp.run(["zsh"])  # Just to make sure the shell is zsh

    subaction("sourcing zsh config file from .mugdot")
    sp.run(["stow", "zsh", "bash"], cwd="/home/mug/.mugdot")
    sp.run(["source", ".zshrc"])


def setup_utils():
    header("Utilities setup")
    sp.run(
        PARU_INSTALL
        + optional_packages["utilities"]
        + optional_packages["documents"]
        + optional_packages["onedrive"]
        + optional_packages["spotify"]
        + optional_packages["terminal stuff"]
        + optional_packages["web"]
    )
    sp.run(["stow", "ncspot"])


def setup_programming():
    header("Programming setup")

    sp.run(
        PARU_INSTALL
        + optional_packages["emacs stuff"]
        + optional_packages["programming tools"]
        + optional_packages["rust"]
        + optional_packages["python"]
        + optional_packages["databases"]
        + optional_packages["sage math"]
        + optional_packages["personal website dev"]
        + optional_packages["latex"]
    )
    sp.run(["rustup", "add", "component"] + optional_packages["rustup components"])

    sp.run(["stow", "alacritty", "emacs", "nvim", "git", "latex", "rust"])
