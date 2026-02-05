#!/usr/bin/env bash
# ==============================================================================
# dotfiles Install Script
# ==============================================================================
# 対話式でツールをインストールし、dotfilesを適用する
# 対応OS: macOS, Linux (Debian/Ubuntu, Fedora, Arch), Windows (WSL, Git Bash)

set -euo pipefail

# ------------------------------------------------------------------------------
# Colors and Formatting
# ------------------------------------------------------------------------------

readonly RED=$'\033[0;31m'
readonly GREEN=$'\033[0;32m'
readonly YELLOW=$'\033[0;33m'
readonly BLUE=$'\033[0;34m'
readonly CYAN=$'\033[0;36m'
readonly BOLD=$'\033[1m'
readonly DIM=$'\033[2m'
readonly NC=$'\033[0m' # No Color

readonly LINE='━'
readonly BOX_WIDTH=60

# ------------------------------------------------------------------------------
# OS Detection
# ------------------------------------------------------------------------------

# OS_TYPE: macos, linux, windows
# PKG_MANAGER: brew, apt, dnf, pacman, winget, scoop
OS_TYPE=""
PKG_MANAGER=""

detect_os() {
    local uname_out
    uname_out="$(uname -s)"

    case "$uname_out" in
        Darwin*)
            OS_TYPE="macos"
            PKG_MANAGER="brew"
            ;;
        Linux*)
            OS_TYPE="linux"
            # Detect package manager
            if command -v apt &>/dev/null; then
                PKG_MANAGER="apt"
            elif command -v dnf &>/dev/null; then
                PKG_MANAGER="dnf"
            elif command -v pacman &>/dev/null; then
                PKG_MANAGER="pacman"
            elif command -v brew &>/dev/null; then
                PKG_MANAGER="brew"
            else
                PKG_MANAGER="unknown"
            fi
            ;;
        MINGW*|MSYS*|CYGWIN*)
            OS_TYPE="windows"
            # Detect package manager
            if command -v winget &>/dev/null; then
                PKG_MANAGER="winget"
            elif command -v scoop &>/dev/null; then
                PKG_MANAGER="scoop"
            else
                PKG_MANAGER="unknown"
            fi
            ;;
        *)
            OS_TYPE="unknown"
            PKG_MANAGER="unknown"
            ;;
    esac
}

# ------------------------------------------------------------------------------
# State Variables
# ------------------------------------------------------------------------------

declare -A INSTALL_DECISIONS=(
    [pkg_manager]=0
    [neovim]=0
    [zellij]=0
    [ghostty]=0
    [amu]=0
    [gh]=0
    [bash_completion]=0
    [claude_code]=0
)

declare -A ALREADY_INSTALLED=(
    [pkg_manager]=0
    [neovim]=0
    [zellij]=0
    [ghostty]=0
    [amu]=0
    [gh]=0
    [bash_completion]=0
    [claude_code]=0
)

APPLY_DOTFILES=0

# ------------------------------------------------------------------------------
# Utility Functions
# ------------------------------------------------------------------------------

print_line() {
    local char="${1:-$LINE}"
    printf '%s' "${CYAN}"
    for ((i = 0; i < BOX_WIDTH; i++)); do
        printf '%s' "$char"
    done
    printf '%s\n' "${NC}"
}

print_header() {
    local title="$1"
    echo
    print_line
    printf " ${BOLD}%s${NC}\n" "$title"
    print_line
}

print_info() {
    printf " ${DIM}%s${NC}\n" "$1"
}

print_note() {
    printf " ${BLUE}📝 %s${NC}\n" "$1"
}

print_warning() {
    printf " ${YELLOW}⚠️  %s${NC}\n" "$1"
}

print_success() {
    printf " ${GREEN}✓ %s${NC}\n" "$1"
}

print_skip() {
    printf " ${DIM}⊘ %s${NC}\n" "$1"
}

print_error() {
    printf " ${RED}✗ %s${NC}\n" "$1"
}

ask_yes_no() {
    local prompt="$1"
    local response

    printf "\n ${prompt} ${DIM}[Y/n]${NC}: "
    read -r response

    case "${response,,}" in
        n|no) return 1 ;;
        *) return 0 ;;
    esac
}

command_exists() {
    command -v "$1" &>/dev/null
}

# Get package manager display name
pkg_manager_name() {
    case "$PKG_MANAGER" in
        brew) echo "Homebrew" ;;
        apt) echo "apt (Debian/Ubuntu)" ;;
        dnf) echo "dnf (Fedora)" ;;
        pacman) echo "pacman (Arch)" ;;
        winget) echo "winget" ;;
        scoop) echo "Scoop" ;;
        *) echo "パッケージマネージャー" ;;
    esac
}

# ------------------------------------------------------------------------------
# Detection Functions
# ------------------------------------------------------------------------------

detect_installed() {
    # Package manager
    case "$PKG_MANAGER" in
        brew)
            if command_exists brew; then
                ALREADY_INSTALLED[pkg_manager]=1
            fi
            ;;
        apt|dnf|pacman)
            # System package managers are always available
            ALREADY_INSTALLED[pkg_manager]=1
            ;;
        winget)
            if command_exists winget; then
                ALREADY_INSTALLED[pkg_manager]=1
            fi
            ;;
        scoop)
            if command_exists scoop; then
                ALREADY_INSTALLED[pkg_manager]=1
            fi
            ;;
    esac

    # Neovim
    if command_exists nvim; then
        ALREADY_INSTALLED[neovim]=1
    fi

    # Zellij
    if command_exists zellij; then
        ALREADY_INSTALLED[zellij]=1
    fi

    # Ghostty (macOS only, check app bundle or command)
    if [[ "$OS_TYPE" == "macos" ]]; then
        if [[ -d "/Applications/Ghostty.app" ]] || command_exists ghostty; then
            ALREADY_INSTALLED[ghostty]=1
        fi
    fi

    # amu
    if command_exists amu; then
        ALREADY_INSTALLED[amu]=1
    fi

    # gh
    if command_exists gh; then
        ALREADY_INSTALLED[gh]=1
    fi

    # bash-completion
    case "$PKG_MANAGER" in
        brew)
            if command_exists brew && brew list bash-completion@2 &>/dev/null; then
                ALREADY_INSTALLED[bash_completion]=1
            fi
            ;;
        apt)
            if dpkg -l bash-completion &>/dev/null 2>&1; then
                ALREADY_INSTALLED[bash_completion]=1
            fi
            ;;
        dnf|pacman)
            # Check if completion file exists
            if [[ -f /usr/share/bash-completion/bash_completion ]]; then
                ALREADY_INSTALLED[bash_completion]=1
            fi
            ;;
    esac

    # Claude Code
    if command_exists claude; then
        ALREADY_INSTALLED[claude_code]=1
    fi
}

# ------------------------------------------------------------------------------
# Installation Prompts
# ------------------------------------------------------------------------------

prompt_pkg_manager() {
    local name
    name="$(pkg_manager_name)"

    print_header "$name"

    case "$OS_TYPE" in
        macos)
            print_info "macOS用パッケージマネージャー"
            ;;
        linux)
            print_info "Linuxシステムパッケージマネージャー"
            ;;
        windows)
            print_info "Windows用パッケージマネージャー"
            ;;
    esac
    echo

    if [[ ${ALREADY_INSTALLED[pkg_manager]} -eq 1 ]]; then
        print_success "インストール済み"
        INSTALL_DECISIONS[pkg_manager]=1
        return 0
    fi

    # Only Homebrew, winget, scoop need installation
    case "$PKG_MANAGER" in
        brew)
            print_warning "これをスキップすると以下もインストールできません:"
            print_info "   Neovim, Zellij, Ghostty, amu, gh, bash-completion"

            if ask_yes_no "インストールしますか？"; then
                INSTALL_DECISIONS[pkg_manager]=1
            else
                print_skip "$name をスキップ（依存ツールも自動スキップ）"
            fi
            ;;
        winget)
            print_info "Microsoft Store経由でインストールできます"
            print_info "または: https://github.com/microsoft/winget-cli"

            if ask_yes_no "wingetのセットアップをスキップしますか？"; then
                print_skip "wingetをスキップ"
            else
                INSTALL_DECISIONS[pkg_manager]=1
            fi
            ;;
        scoop)
            print_info "PowerShellで以下を実行してインストール:"
            print_info "   irm get.scoop.sh | iex"

            if ask_yes_no "Scoopのセットアップをスキップしますか？"; then
                print_skip "Scoopをスキップ"
            else
                INSTALL_DECISIONS[pkg_manager]=1
            fi
            ;;
        apt|dnf|pacman)
            # System package managers don't need installation
            INSTALL_DECISIONS[pkg_manager]=1
            ;;
        *)
            print_error "対応するパッケージマネージャーが見つかりません"
            print_info "手動でインストールしてください"
            ;;
    esac
}

prompt_neovim() {
    if [[ ${INSTALL_DECISIONS[pkg_manager]} -eq 0 && ${ALREADY_INSTALLED[pkg_manager]} -eq 0 ]]; then
        return 0
    fi

    print_header "Neovim"
    print_info "モダンなVimエディタ"
    echo

    if [[ ${ALREADY_INSTALLED[neovim]} -eq 1 ]]; then
        print_success "インストール済み"
        INSTALL_DECISIONS[neovim]=1
        return 0
    fi

    print_note "dotfilesとの関連:"
    print_info "   - EDITOR=nvim として設定"
    print_info "   - vi, vim コマンドが nvim にエイリアス"
    print_info "   - ~/.config/nvim/ に設定ファイルあり"

    if ask_yes_no "インストールしますか？"; then
        INSTALL_DECISIONS[neovim]=1
    fi
}

prompt_zellij() {
    if [[ ${INSTALL_DECISIONS[pkg_manager]} -eq 0 && ${ALREADY_INSTALLED[pkg_manager]} -eq 0 ]]; then
        return 0
    fi

    # Zellij is not available on Windows (Git Bash)
    if [[ "$OS_TYPE" == "windows" ]]; then
        return 0
    fi

    print_header "Zellij"
    print_info "ターミナルマルチプレクサ（tmux代替）"
    echo

    if [[ ${ALREADY_INSTALLED[zellij]} -eq 1 ]]; then
        print_success "インストール済み"
        INSTALL_DECISIONS[zellij]=1
        return 0
    fi

    print_note "dotfilesとの関連:"
    print_info "   - ~/.config/zellij/config.kdl にEmacs風キーバインド設定"
    print_info "   - ミニマルレイアウト設定あり"

    if ask_yes_no "インストールしますか？"; then
        INSTALL_DECISIONS[zellij]=1
    fi
}

prompt_ghostty() {
    # Ghostty is macOS only (cask)
    if [[ "$OS_TYPE" != "macos" ]]; then
        return 0
    fi

    if [[ ${INSTALL_DECISIONS[pkg_manager]} -eq 0 && ${ALREADY_INSTALLED[pkg_manager]} -eq 0 ]]; then
        return 0
    fi

    print_header "Ghostty"
    print_info "高速なターミナルエミュレータ"
    echo

    if [[ ${ALREADY_INSTALLED[ghostty]} -eq 1 ]]; then
        print_success "インストール済み"
        INSTALL_DECISIONS[ghostty]=1
        return 0
    fi

    print_note "dotfilesとの関連:"
    print_info "   - ~/.config/ghostty/config に設定ファイルあり"

    if ask_yes_no "インストールしますか？"; then
        INSTALL_DECISIONS[ghostty]=1
    fi
}

prompt_amu() {
    if [[ ${INSTALL_DECISIONS[pkg_manager]} -eq 0 && ${ALREADY_INSTALLED[pkg_manager]} -eq 0 ]]; then
        return 0
    fi

    print_header "amu"
    print_info "シンボリックリンク管理ツール"
    echo

    if [[ ${ALREADY_INSTALLED[amu]} -eq 1 ]]; then
        print_success "インストール済み"
        INSTALL_DECISIONS[amu]=1
        return 0
    fi

    print_note "dotfilesとの関連:"
    print_info "   - dotfilesの適用（シンボリックリンク作成）に必要"
    print_info "   - amu add ~/dot/dotfiles/HOME ~/ で設定を反映"

    # Show install method based on OS
    case "$PKG_MANAGER" in
        brew)
            print_info "   - brew install amu"
            ;;
        *)
            print_info "   - cargo install amu（Rust必要）"
            ;;
    esac

    if ask_yes_no "インストールしますか？"; then
        INSTALL_DECISIONS[amu]=1
    fi
}

prompt_gh() {
    if [[ ${INSTALL_DECISIONS[pkg_manager]} -eq 0 && ${ALREADY_INSTALLED[pkg_manager]} -eq 0 ]]; then
        return 0
    fi

    print_header "gh (GitHub CLI)"
    print_info "GitHub操作用CLI（オプション）"
    echo

    if [[ ${ALREADY_INSTALLED[gh]} -eq 1 ]]; then
        print_success "インストール済み"
        INSTALL_DECISIONS[gh]=1
        return 0
    fi

    print_note "用途:"
    print_info "   - PRやIssueの操作"
    print_info "   - リポジトリのクローン（gh repo clone）"

    if ask_yes_no "インストールしますか？"; then
        INSTALL_DECISIONS[gh]=1
    fi
}

prompt_bash_completion() {
    # Skip if zsh is the default shell
    if [[ "$SHELL" == */zsh ]]; then
        return 0
    fi

    # Skip on Windows
    if [[ "$OS_TYPE" == "windows" ]]; then
        return 0
    fi

    if [[ ${INSTALL_DECISIONS[pkg_manager]} -eq 0 && ${ALREADY_INSTALLED[pkg_manager]} -eq 0 ]]; then
        return 0
    fi

    print_header "bash-completion"
    print_info "bashのタブ補完強化（オプション）"
    echo

    if [[ ${ALREADY_INSTALLED[bash_completion]} -eq 1 ]]; then
        print_success "インストール済み"
        INSTALL_DECISIONS[bash_completion]=1
        return 0
    fi

    print_note "機能:"
    print_info "   - git, brew 等のサブコマンドをタブ補完"
    print_info "   - 例: git ch<TAB> → checkout, cherry-pick..."
    echo
    print_note "dotfilesとの関連:"
    print_info "   - .bashrcで自動読み込み設定あり"

    if ask_yes_no "インストールしますか？"; then
        INSTALL_DECISIONS[bash_completion]=1
    fi
}

prompt_claude_code() {
    print_header "Claude Code"
    print_info "Anthropic AI CLI"
    echo

    if [[ ${ALREADY_INSTALLED[claude_code]} -eq 1 ]]; then
        print_success "インストール済み"
        INSTALL_DECISIONS[claude_code]=1
        return 0
    fi

    print_note "dotfilesとの関連:"
    print_info "   - fclaude エイリアスが設定されています"
    print_info "   - 公式インストーラーを使用"

    if ask_yes_no "インストールしますか？"; then
        INSTALL_DECISIONS[claude_code]=1
    fi
}

prompt_apply_dotfiles() {
    print_header "dotfilesの適用"

    if [[ ${INSTALL_DECISIONS[amu]} -eq 0 && ${ALREADY_INSTALLED[amu]} -eq 0 ]]; then
        print_warning "amuがインストールされないため、手動でリンクする必要があります"
        print_info "   手動コマンド例:"
        print_info "   ln -s ~/dot/dotfiles/HOME/.bashrc ~/.bashrc"
        return 0
    fi

    print_info "amuを使用してdotfilesをホームディレクトリにリンクします"
    print_info "   コマンド: amu add ~/dot/dotfiles/HOME ~/"
    echo
    print_warning "既存ファイルは上書きされる可能性があります"

    if ask_yes_no "dotfilesを適用しますか？"; then
        APPLY_DOTFILES=1
    fi
}

# ------------------------------------------------------------------------------
# Confirmation
# ------------------------------------------------------------------------------

show_summary() {
    print_header "インストール内容の確認"
    echo

    local install_list=()
    local skip_list=()
    local name

    # Package manager
    name="$(pkg_manager_name)"
    if [[ ${ALREADY_INSTALLED[pkg_manager]} -eq 1 ]]; then
        : # Already installed
    elif [[ ${INSTALL_DECISIONS[pkg_manager]} -eq 1 ]]; then
        install_list+=("$name")
    elif [[ "$PKG_MANAGER" == "brew" || "$PKG_MANAGER" == "winget" || "$PKG_MANAGER" == "scoop" ]]; then
        skip_list+=("$name")
    fi

    # Tools
    for pkg in neovim zellij ghostty amu gh bash_completion; do
        # Skip conditions
        [[ "$pkg" == "bash_completion" && "$SHELL" == */zsh ]] && continue
        [[ "$pkg" == "bash_completion" && "$OS_TYPE" == "windows" ]] && continue
        [[ "$pkg" == "ghostty" && "$OS_TYPE" != "macos" ]] && continue
        [[ "$pkg" == "zellij" && "$OS_TYPE" == "windows" ]] && continue

        local display_name
        case "$pkg" in
            bash_completion) display_name="bash-completion" ;;
            *) display_name="$pkg" ;;
        esac

        if [[ ${ALREADY_INSTALLED[$pkg]} -eq 1 ]]; then
            : # Already installed
        elif [[ ${INSTALL_DECISIONS[$pkg]} -eq 1 ]]; then
            install_list+=("$display_name")
        elif [[ ${INSTALL_DECISIONS[pkg_manager]} -eq 1 || ${ALREADY_INSTALLED[pkg_manager]} -eq 1 ]]; then
            skip_list+=("$display_name")
        fi
    done

    # Claude Code
    if [[ ${ALREADY_INSTALLED[claude_code]} -eq 1 ]]; then
        : # Already installed
    elif [[ ${INSTALL_DECISIONS[claude_code]} -eq 1 ]]; then
        install_list+=("Claude Code")
    else
        skip_list+=("Claude Code")
    fi

    # Show lists
    if [[ ${#install_list[@]} -gt 0 ]]; then
        printf " ${GREEN}インストール:${NC}\n"
        for item in "${install_list[@]}"; do
            printf "   ${GREEN}✓${NC} %s\n" "$item"
        done
        echo
    fi

    if [[ ${#skip_list[@]} -gt 0 ]]; then
        printf " ${DIM}スキップ:${NC}\n"
        for item in "${skip_list[@]}"; do
            printf "   ${DIM}⊘ %s${NC}\n" "$item"
        done
        echo
    fi

    if [[ $APPLY_DOTFILES -eq 1 ]]; then
        printf " ${BLUE}dotfiles適用:${NC} amu add\n"
        echo
    fi

    if [[ ${#install_list[@]} -eq 0 && $APPLY_DOTFILES -eq 0 ]]; then
        print_info "インストールする項目がありません"
        return 1
    fi

    return 0
}

confirm_execution() {
    if ! ask_yes_no "実行しますか？"; then
        echo
        print_info "キャンセルしました"
        exit 0
    fi
}

# ------------------------------------------------------------------------------
# Installation Functions
# ------------------------------------------------------------------------------

install_pkg_manager() {
    if [[ ${ALREADY_INSTALLED[pkg_manager]} -eq 1 ]]; then
        return 0
    fi
    if [[ ${INSTALL_DECISIONS[pkg_manager]} -eq 0 ]]; then
        return 0
    fi

    case "$PKG_MANAGER" in
        brew)
            print_header "Homebrew をインストール中..."
            /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

            # Add to PATH for this session
            if [[ -d "/opt/homebrew/bin" ]]; then
                export PATH="/opt/homebrew/bin:$PATH"
            elif [[ -d "/home/linuxbrew/.linuxbrew/bin" ]]; then
                export PATH="/home/linuxbrew/.linuxbrew/bin:$PATH"
            fi

            print_success "Homebrew インストール完了"
            ;;
        scoop)
            print_header "Scoop をインストール中..."
            print_info "PowerShellで以下を実行してください:"
            print_info "   irm get.scoop.sh | iex"
            print_warning "このスクリプトからは自動インストールできません"
            ;;
        winget)
            print_header "winget のセットアップ"
            print_info "Microsoft Storeから「アプリ インストーラー」をインストールしてください"
            print_warning "このスクリプトからは自動インストールできません"
            ;;
    esac
}

install_package() {
    local key="$1"
    local pkg_brew="${2:-}"
    local pkg_apt="${3:-}"
    local pkg_dnf="${4:-}"
    local pkg_pacman="${5:-}"
    local pkg_winget="${6:-}"
    local pkg_scoop="${7:-}"
    local is_cask="${8:-false}"

    if [[ ${ALREADY_INSTALLED[$key]} -eq 1 ]]; then
        return 0
    fi
    if [[ ${INSTALL_DECISIONS[$key]} -eq 0 ]]; then
        return 0
    fi

    local package=""
    case "$PKG_MANAGER" in
        brew) package="$pkg_brew" ;;
        apt) package="$pkg_apt" ;;
        dnf) package="$pkg_dnf" ;;
        pacman) package="$pkg_pacman" ;;
        winget) package="$pkg_winget" ;;
        scoop) package="$pkg_scoop" ;;
    esac

    if [[ -z "$package" ]]; then
        print_warning "$key: このOSでは自動インストールできません"
        return 0
    fi

    echo
    printf " ${CYAN}→${NC} %s をインストール中...\n" "$package"

    local success=false
    case "$PKG_MANAGER" in
        brew)
            if [[ "$is_cask" == "true" ]]; then
                brew install --cask "$package" && success=true
            else
                brew install "$package" && success=true
            fi
            ;;
        apt)
            sudo apt update && sudo apt install -y "$package" && success=true
            ;;
        dnf)
            sudo dnf install -y "$package" && success=true
            ;;
        pacman)
            sudo pacman -S --noconfirm "$package" && success=true
            ;;
        winget)
            winget install -e --id "$package" && success=true
            ;;
        scoop)
            scoop install "$package" && success=true
            ;;
    esac

    if [[ "$success" == "true" ]]; then
        print_success "$package インストール完了"
    else
        print_error "$package のインストールに失敗しました"
    fi
}

install_amu() {
    if [[ ${ALREADY_INSTALLED[amu]} -eq 1 ]]; then
        return 0
    fi
    if [[ ${INSTALL_DECISIONS[amu]} -eq 0 ]]; then
        return 0
    fi

    echo
    printf " ${CYAN}→${NC} amu をインストール中...\n"

    local success=false
    case "$PKG_MANAGER" in
        brew)
            brew install amu && success=true
            ;;
        *)
            # Try cargo install
            if command_exists cargo; then
                cargo install amu && success=true
            else
                print_warning "amuのインストールにはRust (cargo) が必要です"
                print_info "   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh"
                print_info "   cargo install amu"
            fi
            ;;
    esac

    if [[ "$success" == "true" ]]; then
        print_success "amu インストール完了"
    else
        print_error "amu のインストールに失敗しました"
    fi
}

install_claude_code() {
    if [[ ${ALREADY_INSTALLED[claude_code]} -eq 1 ]]; then
        return 0
    fi
    if [[ ${INSTALL_DECISIONS[claude_code]} -eq 0 ]]; then
        return 0
    fi

    print_header "Claude Code をインストール中..."

    if [[ "$OS_TYPE" == "windows" ]]; then
        print_info "Windows版のインストール方法（PowerShellで実行）:"
        print_info "   irm https://claude.ai/install.ps1 | iex"
        print_warning "このスクリプトからは自動インストールできません"
    else
        curl -fsSL https://claude.ai/install.sh | bash
        print_success "Claude Code インストール完了"
    fi
}

apply_dotfiles() {
    if [[ $APPLY_DOTFILES -eq 0 ]]; then
        return 0
    fi

    print_header "dotfilesを適用中..."

    local dotfiles_dir
    dotfiles_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    local home_source="${dotfiles_dir}/HOME"

    if [[ ! -d "$home_source" ]]; then
        print_error "HOME ディレクトリが見つかりません: $home_source"
        return 1
    fi

    if command_exists amu; then
        if amu add "$home_source" ~/; then
            print_success "dotfiles 適用完了"
            echo
            print_note "変更を反映するには、新しいシェルを起動するか以下を実行:"
            print_info "   source ~/.bashrc"
        else
            print_error "dotfiles の適用に失敗しました"
        fi
    else
        print_error "amu コマンドが見つかりません"
        print_info "手動でシンボリックリンクを作成してください"
    fi
}

# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------

main() {
    echo
    printf "${BOLD}${CYAN}"
    echo "  ╔══════════════════════════════════════════════════════════╗"
    echo "  ║            dotfiles インストールスクリプト               ║"
    echo "  ╚══════════════════════════════════════════════════════════╝"
    printf "${NC}\n"

    # Detect OS and package manager
    detect_os

    printf " ${DIM}OS: %s | パッケージマネージャー: %s${NC}\n" "$OS_TYPE" "$(pkg_manager_name)"

    if [[ "$OS_TYPE" == "unknown" ]]; then
        print_error "サポートされていないOSです"
        exit 1
    fi

    # Detect already installed tools
    detect_installed

    # Collect installation decisions
    prompt_pkg_manager
    prompt_neovim
    prompt_zellij
    prompt_ghostty
    prompt_amu
    prompt_gh
    prompt_bash_completion
    prompt_claude_code
    prompt_apply_dotfiles

    # Show summary and confirm
    echo
    if ! show_summary; then
        echo
        print_success "全てのツールがインストール済みです"
        exit 0
    fi

    confirm_execution

    # Execute installations
    print_header "インストール実行"

    install_pkg_manager

    # Install packages: key, brew, apt, dnf, pacman, winget, scoop, is_cask
    install_package "neovim" "neovim" "neovim" "neovim" "neovim" "Neovim.Neovim" "neovim"
    install_package "zellij" "zellij" "" "" "zellij" "" ""  # Linux: cargo or manual
    install_package "ghostty" "ghostty" "" "" "" "" "" "true"  # macOS only
    install_amu
    install_package "gh" "gh" "gh" "gh" "github-cli" "GitHub.cli" "gh"
    install_package "bash_completion" "bash-completion@2" "bash-completion" "bash-completion" "bash-completion" "" ""

    install_claude_code
    apply_dotfiles

    # Done
    echo
    print_line
    printf " ${GREEN}${BOLD}完了！${NC}\n"
    print_line
    echo
}

# Run
main "$@"
