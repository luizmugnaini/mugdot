from pathlib import Path
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
    ],
    "xorg": [  # NOTE: X11 only
        "xorg-server",
        "xorg-xsetroot",
        "xorg-xrandr",
        "xorg-xmodmap",
        "xorg-xset",
        "xorg-setxkbmap",
        "xorg-utils",
        "xorg-xinit",
        "xclip",
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
    "compositor": ["picom"],  # TODO: This is an X compositor, should change to hyprland
    "fonts": [
        "nerd-fonts-complete",
        "ttf-nerd-fonts-symbols",
        "ttf-apple-emoji",
        "adobe-source-code-pro-fonts",
        "ttf-iosevka-nerd",
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
    "dotfiles setup": ["stoic-dotfiles"],  # Installed via cargo
    "python": [
        "python-poetry",
        "pyright",
        "ipython",
        "jupyterlab",
        "tk",
    ],
    "programming tools": ["tree-sitter"],
    "personal website dev": ["zola"],
    "sage math": ["sagemath"],
    "databases": ["postgresql"],  # TODO: setup database
    "latex": [
        # Core
        "texlive-basic",
        # Binaries
        "texlive-bin",
        "texlive-binextra",
        # Packages
        "texlive-latex",
        "texlive-latexrecommended",
        "texlive-latexextra",
        # Fonts
        "texlive-fontsrecommended",
        "texlive-fontsextra",
        # Language support
        "texlive-langportuguese",
        "texlive-langjapanese",
        # Bibliography
        "biber",
    ],
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
        "eza",
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

PACMAN_INSTALL = ["sudo", "pacman", "-S"]
PARU_INSTALL = ["paru", "-S"]


def header(action: str):
    action = " " + action + " "
    print(f"\n{action:-^80}\n")


def subaction(subact: str):
    indicator = "\033[1;32m::\033[0m"
    print(f"\n{indicator} {subact}\n")


def initial_setup():
    header("User creation")

    subaction("Creating user")
    name = input("Name of the user: ")
    sp.run(["useradd", "m", "-g", "users", "-G", "wheel", name])

    subaction("Creating a password for the user 'mug'")
    sp.run(["passwd", "mug"])


def setup_pacman():
    header("Setup pacman")
    subaction("Initialising pacman keys")
    sp.run(["sudo", "pacman-key", "--init"], cwd=HOME)

    subaction("Populating pacman keys")
    sp.run(["sudo", "pacman-key" "--populate archlinux"], cwd=HOME)


def setup_audio():
    header("Setup audio")

    subaction("Installing audio packages")
    sp.run(PACMAN_INSTALL + compulsory_packages["audio"], cwd=HOME)

    subaction("Copying pipewire configuration into /etc/pipewire")
    sp.run(["sudo", "mkdir", "/etc/pipewire"])
    sp.run(["sudo", "cp", "/usr/share/pipewire/pipewire*", "/etc/pipewire/"])

    subaction("Enabling/starting pipewire and pipewire-pulse")
    sp.run(["systemctl", "--user", "enable", "pipewire{,-pulse}"])
    sp.run(["systemctl", "--user", "start", "pipewire{,-pulse}"])


def setup_rust():
    header("Setup rust")

    subaction("Installing rust")
    sp.run(PACMAN_INSTALL + compulsory_packages["rust"])

    subaction("Setting up rustup with the stable toolchain")
    sp.run(["rustup", "default", "stable"])


def setup_bluetooth():
    header("Setup bluetooth")

    subaction("Installing bluetooth packages")
    sp.run(PACMAN_INSTALL + compulsory_packages["bluetooth"], cwd=HOME)

    subaction("Enabling and starting the bluetooth service")
    sp.run(["sudo", "systemctl", "enable", "bluetooth"])
    sp.run(["sudo", "systemctl", "start", "bluetooth"])


def setup_display_manager():
    header("Setup the display manager")

    subaction("Installing the display manager packages")
    sp.run(PACMAN_INSTALL + compulsory_packages["display manager"], cwd=HOME)

    subaction("Enabling/starting lightdm")
    sp.run(["sudo", "systemctl", "enable", "lightdm"], cwd=HOME)
    sp.run(["sudo", "systemctl", "start", "lightdm"], cwd=HOME)


# TODO: break this into multiple functions
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
    sp.run(["rustup", "default", "stable"])
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
    )

    # Display manager
    sp.run(["sudo", "systemctl", "enable", "lightdm"])
    sp.run(["sudo", "systemctl", "start", "lightdm"])

    # Bluetooth
    sp.run(["sudo", "systemctl", "enable", "bluetooth"])
    sp.run(["sudo", "systemctl", "start", "bluetooth"])

    sp.run(["stow", "x"])


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


def setup_network():
    header("Network setup")

    header("Installing networking packages")
    sp.run(["sudo", "pacman", "-S"] + compulsory_packages["networking"], cwd=HOME)

    subaction("Setup dhcp to dhclient")
    config_path = Path("/etc/NetworkManager/conf.d")
    content = sp.Popen(["printf", "[main]\ndhcp=dhclient"], stdout=sp.PIPE)
    sp.run(
        ["sudo", "tee", "dhcp-client.conf"],
        stdin=content.stdout,
        cwd=config_path,
        stdout=sp.DEVNULL,
    )

    subaction("Setting wifi backend to iwd")
    content = sp.Popen(["printf", "[device]\nwifi.backend=iwd"], stdout=sp.PIPE)
    sp.run(
        ["sudo", "tee", "wifi_backend.conf"],
        stdin=content.stdout,
        cwd=config_path,
        stdout=sp.DEVNULL,
    )

    subaction("Enabling both the NetworkManager and iwd")
    sp.run(["sudo", "systemctl", "enable", "NetworkManager"], cwd=HOME)
    sp.run(["sudo", "systemctl", "enable", "iwd"], cwd=HOME)

    subaction("Starting both the NetworkManager and iwd")
    sp.run(["sudo", "systemctl", "start", "NetworkManager"], cwd=HOME)
    sp.run(["sudo", "systemctl", "start", "iwd"], cwd=HOME)


def setup_paru():
    header("Paru AUR helper setup")

    subaction("Installing base-devel (basic tools to build Arch Linux packages)")
    sp.run(["sudo", "pacman", "-S", "--needed", "base-devel"], cwd=HOME)

    subaction("Installing rust")
    sp.run(["sudo", "pacman", "-S"] + compulsory_packages["rust"])
    sp.run(["rustup", "default", "stable"])

    subaction("Installing git")
    sp.run(["sudo", "pacman", "-S", "git"], cwd=HOME)

    subaction("Cloning paru")
    sp.run(["git", "clone", "https://aur.archlinux.org/paru.git"], cwd=HOME)

    subaction("Making paru")
    sp.run(["makepkg", "-si"], cwd=HOME / "paru")

    subaction("Deleting paru repository")
    sp.run(["rm", "-rf", "paru"], cwd=HOME)

    subaction("Installing perl")
    sp.run(["sudo", "pacman", "-S", "perl"])

    # NOTE: uses https://unix.stackexchange.com/a/26289
    subaction("Uncommenting the multilib mirror from /etc/pacman.conf")
    sp.run(
        [
            "perl",
            "-0777",
            "-pi",
            "-e",
            "s/#\\[multilib\\]\n#Include = \\/etc\\/pacman.d\\/mirrorlist/\\[multilib\\]\nInclude = \\/etc\\/pacman.d\\/mirrorlist/g",
            "/etc/pacman.conf",
        ],
        cwd=HOME,
    )

    subaction("Updating paru mirrors")
    sp.run(["paru", "-Syy"], cwd=HOME)


def setup_dwm():
    header("Setup dwm window manager")
    sp.run(PARU_INSTALL + optional_packages["dwm stuff"])

    common_path = "/home/mug/.mugdot/dwm/.config/"
    sp.run(["sudo", "make", "clean", "install"], cwd=common_path + "dwm-6.2")

    sp.run("make", cwd=common_path + "dwmblocks")
    sp.run(["sudo", "make", "install"], cwd=common_path + "dwmblocks")
    sp.run(["stow", "dunst", "rofi"])
    # TODO: create desktop entry for dwm


def setup_shell():
    header("Setup shell")

    subaction("Installing zhs packages")
    sp.run(PARU_INSTALL + optional_packages["shell"])

    subaction("Make zsh the default shell")
    sp.run(["chsh", "-s", "/bin/zsh"])

    subaction("Switching to the zsh shell")
    sp.run(["zsh"])

    subaction("Sourcing zsh config file from .mugdot")
    sp.run(["stow", "zsh", "bash"], cwd=DOTFILES_DIR)
    sp.run(["source", ".zshrc"], cwd=HOME)


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
    sp.run(
        ["rustup", "add", "component"] + optional_packages["rustup components"],
        cwd=HOME,
    )

    sp.run(
        ["stow", "alacritty", "emacs", "nvim", "git", "latex", "rust"],
        cwd=DOTFILES_DIR,
    )


if __name__ == "__main__":
    HOME = Path.home()
    DOTFILES_DIR = HOME / ".mugdot"
