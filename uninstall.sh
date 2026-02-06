#!/usr/bin/env bash
# ==============================================================================
# dotfiles Uninstall Script
# ==============================================================================
# 対話式でdotfilesとツールを削除する
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

declare -A UNINSTALL_DECISIONS=(
    [neovim]=0
    [zellij]=0
    [ghostty]=0
    [amu]=0
    [gh]=0
    [glow]=0
    [fzf]=0
    [fd]=0
    [wtp]=0
    [starship]=0
    [bash_completion]=0
    [claude_code]=0
)

declare -A IS_INSTALLED=(
    [neovim]=0
    [zellij]=0
    [ghostty]=0
    [amu]=0
    [gh]=0
    [glow]=0
    [fzf]=0
    [fd]=0
    [wtp]=0
    [starship]=0
    [bash_completion]=0
    [claude_code]=0
)

REMOVE_DOTFILES=0

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
    local default="${2:-y}"
    local response

    if [[ "$default" == "n" ]]; then
        printf "\n ${prompt} ${DIM}[y/N]${NC}: "
    else
        printf "\n ${prompt} ${DIM}[Y/n]${NC}: "
    fi
    read -r response

    if [[ "$default" == "n" ]]; then
        case "${response,,}" in
            y|yes) return 0 ;;
            *) return 1 ;;
        esac
    else
        case "${response,,}" in
            n|no) return 1 ;;
            *) return 0 ;;
        esac
    fi
}

command_exists() {
    command -v "$1" &>/dev/null
}

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
    # Neovim
    if command_exists nvim; then
        IS_INSTALLED[neovim]=1
    fi

    # Zellij
    if command_exists zellij; then
        IS_INSTALLED[zellij]=1
    fi

    # Ghostty (macOS only)
    if [[ "$OS_TYPE" == "macos" ]]; then
        if [[ -d "/Applications/Ghostty.app" ]] || command_exists ghostty; then
            IS_INSTALLED[ghostty]=1
        fi
    fi

    # amu
    if command_exists amu; then
        IS_INSTALLED[amu]=1
    fi

    # gh
    if command_exists gh; then
        IS_INSTALLED[gh]=1
    fi

    # glow
    if command_exists glow; then
        IS_INSTALLED[glow]=1
    fi

    # fzf
    if command_exists fzf; then
        IS_INSTALLED[fzf]=1
    fi

    # fd (fd-find on Debian/Ubuntu)
    if command_exists fd || command_exists fdfind; then
        IS_INSTALLED[fd]=1
    fi

    # wtp
    if command_exists wtp; then
        IS_INSTALLED[wtp]=1
    fi

    # starship
    if command_exists starship; then
        IS_INSTALLED[starship]=1
    fi

    # bash-completion
    case "$PKG_MANAGER" in
        brew)
            if command_exists brew && brew list bash-completion@2 &>/dev/null; then
                IS_INSTALLED[bash_completion]=1
            fi
            ;;
        apt)
            if dpkg -l bash-completion &>/dev/null 2>&1; then
                IS_INSTALLED[bash_completion]=1
            fi
            ;;
        dnf|pacman)
            if [[ -f /usr/share/bash-completion/bash_completion ]]; then
                IS_INSTALLED[bash_completion]=1
            fi
            ;;
    esac

    # Claude Code
    if command_exists claude; then
        IS_INSTALLED[claude_code]=1
    fi
}

# ------------------------------------------------------------------------------
# Uninstall Prompts
# ------------------------------------------------------------------------------

prompt_remove_dotfiles() {
    print_header "dotfilesのリンク解除"

    if ! command_exists amu; then
        print_warning "amuがインストールされていません"
        print_info "   手動でシンボリックリンクを削除してください"
        return 0
    fi

    print_info "amuで作成したシンボリックリンクを削除します"
    print_info "   コマンド: amu rm ~/dot/dotfiles/HOME ~/"
    echo
    print_warning "元の設定ファイルは復元されません"

    if ask_yes_no "dotfilesのリンクを解除しますか？" "n"; then
        REMOVE_DOTFILES=1
    fi
}

prompt_uninstall_tool() {
    local key="$1"
    local name="$2"
    local description="$3"

    if [[ ${IS_INSTALLED[$key]} -eq 0 ]]; then
        return 0
    fi

    print_header "$name"
    print_info "$description"
    echo

    print_success "インストール済み"

    if ask_yes_no "アンインストールしますか？" "n"; then
        UNINSTALL_DECISIONS[$key]=1
    fi
}

# ------------------------------------------------------------------------------
# Confirmation
# ------------------------------------------------------------------------------

show_summary() {
    print_header "削除内容の確認"
    echo

    local remove_list=()

    if [[ $REMOVE_DOTFILES -eq 1 ]]; then
        remove_list+=("dotfilesリンク")
    fi

    for pkg in neovim zellij ghostty amu gh glow fzf fd wtp starship bash_completion claude_code; do
        if [[ ${UNINSTALL_DECISIONS[$pkg]} -eq 1 ]]; then
            local display_name
            case "$pkg" in
                bash_completion) display_name="bash-completion" ;;
                claude_code) display_name="Claude Code" ;;
                *) display_name="$pkg" ;;
            esac
            remove_list+=("$display_name")
        fi
    done

    if [[ ${#remove_list[@]} -eq 0 ]]; then
        print_info "削除する項目がありません"
        return 1
    fi

    printf " ${RED}削除:${NC}\n"
    for item in "${remove_list[@]}"; do
        printf "   ${RED}✗${NC} %s\n" "$item"
    done
    echo

    return 0
}

confirm_execution() {
    print_warning "この操作は取り消せません"

    if ! ask_yes_no "本当に実行しますか？" "n"; then
        echo
        print_info "キャンセルしました"
        exit 0
    fi
}

# ------------------------------------------------------------------------------
# Uninstall Functions
# ------------------------------------------------------------------------------

remove_dotfiles() {
    if [[ $REMOVE_DOTFILES -eq 0 ]]; then
        return 0
    fi

    print_header "dotfilesのリンクを解除中..."

    local dotfiles_dir
    dotfiles_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    local home_source="${dotfiles_dir}/HOME"

    if [[ ! -d "$home_source" ]]; then
        print_error "HOME ディレクトリが見つかりません: $home_source"
        return 1
    fi

    if command_exists amu; then
        if amu rm "$home_source" ~/; then
            print_success "dotfilesリンク解除完了"
        else
            print_error "dotfilesリンク解除に失敗しました"
        fi
    else
        print_error "amu コマンドが見つかりません"
    fi
}

uninstall_package() {
    local key="$1"
    local pkg_brew="${2:-}"
    local pkg_apt="${3:-}"
    local pkg_dnf="${4:-}"
    local pkg_pacman="${5:-}"
    local pkg_winget="${6:-}"
    local pkg_scoop="${7:-}"
    local is_cask="${8:-false}"

    if [[ ${UNINSTALL_DECISIONS[$key]} -eq 0 ]]; then
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
        print_warning "$key: このOSでは自動アンインストールできません"
        return 0
    fi

    echo
    printf " ${CYAN}→${NC} %s をアンインストール中...\n" "$package"

    local success=false
    case "$PKG_MANAGER" in
        brew)
            if [[ "$is_cask" == "true" ]]; then
                brew uninstall --cask "$package" && success=true
            else
                brew uninstall "$package" && success=true
            fi
            ;;
        apt)
            sudo apt remove -y "$package" && success=true
            ;;
        dnf)
            sudo dnf remove -y "$package" && success=true
            ;;
        pacman)
            sudo pacman -Rs --noconfirm "$package" && success=true
            ;;
        winget)
            winget uninstall -e --id "$package" && success=true
            ;;
        scoop)
            scoop uninstall "$package" && success=true
            ;;
    esac

    if [[ "$success" == "true" ]]; then
        print_success "$package アンインストール完了"
    else
        print_error "$package のアンインストールに失敗しました"
    fi
}

uninstall_amu() {
    if [[ ${UNINSTALL_DECISIONS[amu]} -eq 0 ]]; then
        return 0
    fi

    echo
    printf " ${CYAN}→${NC} amu をアンインストール中...\n"

    local success=false
    case "$PKG_MANAGER" in
        brew)
            brew uninstall amu && success=true
            ;;
        *)
            if command_exists cargo; then
                cargo uninstall amu && success=true
            else
                print_warning "cargoが見つかりません。手動で削除してください"
                print_info "   rm ~/.cargo/bin/amu"
            fi
            ;;
    esac

    if [[ "$success" == "true" ]]; then
        print_success "amu アンインストール完了"
    fi
}

uninstall_wtp() {
    if [[ ${UNINSTALL_DECISIONS[wtp]} -eq 0 ]]; then
        return 0
    fi

    echo
    printf " ${CYAN}→${NC} wtp をアンインストール中...\n"

    if brew uninstall wtp; then
        print_success "wtp アンインストール完了"
    else
        print_error "wtp のアンインストールに失敗しました"
    fi
}

uninstall_claude_code() {
    if [[ ${UNINSTALL_DECISIONS[claude_code]} -eq 0 ]]; then
        return 0
    fi

    echo
    printf " ${CYAN}→${NC} Claude Code をアンインストール中...\n"

    # Claude Code is typically installed via npm
    if command_exists npm; then
        if npm uninstall -g @anthropic-ai/claude-code 2>/dev/null; then
            print_success "Claude Code アンインストール完了"
            return 0
        fi
    fi

    # Try removing from common locations
    local claude_path
    claude_path="$(command -v claude 2>/dev/null || true)"
    if [[ -n "$claude_path" ]]; then
        print_info "Claude Code のパス: $claude_path"
        print_warning "手動で削除してください:"
        print_info "   rm -rf $claude_path"
        print_info "   rm -rf ~/.claude"
    else
        print_warning "Claude Code の場所が特定できません"
    fi
}

# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------

main() {
    echo
    printf "${BOLD}${CYAN}"
    echo "  ╔══════════════════════════════════════════════════════════╗"
    echo "  ║          dotfiles アンインストールスクリプト             ║"
    echo "  ╚══════════════════════════════════════════════════════════╝"
    printf "${NC}\n"

    # Detect OS and package manager
    detect_os

    printf " ${DIM}OS: %s | パッケージマネージャー: %s${NC}\n" "$OS_TYPE" "$(pkg_manager_name)"

    if [[ "$OS_TYPE" == "unknown" ]]; then
        print_error "サポートされていないOSです"
        exit 1
    fi

    # Detect installed tools
    detect_installed

    # Collect uninstall decisions
    prompt_remove_dotfiles
    prompt_uninstall_tool "neovim" "Neovim" "モダンなVimエディタ"
    prompt_uninstall_tool "zellij" "Zellij" "ターミナルマルチプレクサ"

    if [[ "$OS_TYPE" == "macos" ]]; then
        prompt_uninstall_tool "ghostty" "Ghostty" "高速なターミナルエミュレータ"
    fi

    prompt_uninstall_tool "gh" "gh (GitHub CLI)" "GitHub操作用CLI"
    prompt_uninstall_tool "glow" "glow" "ターミナル用Markdownビューア"
    prompt_uninstall_tool "fzf" "fzf" "コマンドラインファジーファインダー"
    prompt_uninstall_tool "fd" "fd" "高速なfind代替コマンド"
    prompt_uninstall_tool "wtp" "wtp" "Git worktree 管理ツール"
    prompt_uninstall_tool "starship" "Starship" "クロスシェルプロンプト"
    prompt_uninstall_tool "bash_completion" "bash-completion" "bashのタブ補完強化"
    prompt_uninstall_tool "claude_code" "Claude Code" "Anthropic AI CLI"

    # amu is last because it's needed for removing dotfiles
    prompt_uninstall_tool "amu" "amu" "シンボリックリンク管理ツール"

    # Show summary and confirm
    echo
    if ! show_summary; then
        echo
        print_info "何も削除しませんでした"
        exit 0
    fi

    confirm_execution

    # Execute uninstalls
    print_header "アンインストール実行"

    # Remove dotfiles first (needs amu)
    remove_dotfiles

    # Uninstall packages: key, brew, apt, dnf, pacman, winget, scoop, is_cask
    uninstall_package "neovim" "neovim" "neovim" "neovim" "neovim" "Neovim.Neovim" "neovim"
    uninstall_package "zellij" "zellij" "" "" "zellij" "" ""
    uninstall_package "ghostty" "ghostty" "" "" "" "" "" "true"
    uninstall_package "gh" "gh" "gh" "gh" "github-cli" "GitHub.cli" "gh"
    uninstall_package "glow" "glow" "" "" "glow" "charmbracelet.glow" "glow"
    uninstall_package "fzf" "fzf" "fzf" "fzf" "fzf" "junegunn.fzf" "fzf"
    uninstall_package "fd" "fd" "fd-find" "fd-find" "fd" "sharkdp.fd" "fd"
    uninstall_wtp
    uninstall_package "starship" "starship" "" "" "starship" "Starship.Starship" "starship"
    uninstall_package "bash_completion" "bash-completion@2" "bash-completion" "bash-completion" "bash-completion" "" ""

    uninstall_claude_code
    uninstall_amu  # Last, after dotfiles removal

    # Done
    echo
    print_line
    printf " ${GREEN}${BOLD}完了！${NC}\n"
    print_line
    echo

    print_info "注意: パッケージマネージャー自体は削除されていません"
    print_info "設定ファイル (~/.config/*) も残っています"
}

# Run
main "$@"
