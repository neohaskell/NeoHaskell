import argparse
import os
from pathlib import Path
import select
import sys
import termios
import time
import tty

from rich import box
from rich.console import Console, Group
from rich.live import Live
from rich.padding import Padding
from rich.panel import Panel
from rich.progress_bar import ProgressBar
from rich.table import Table
from rich.text import Text


PINK = '#f39bc4'
LILAC = '#c6a0ff'
MINT = '#9ee6bc'
MUTED = '#b5a9c9'
SURFACE = '#1c1826'
WHITE = '#f5efff'
VARIANTS = {'1': 'A. Ribbon', '2': 'B. Studio', '3': 'C. Dashboard'}
SCENARIOS = {
    'new': 'Fresh install',
    'existing': 'Compatible Nix already installed',
    'network': 'Connection lost while downloading neoctl',
    'systemd': 'WSL2 without active systemd',
    'managed': 'Managed configuration cannot be located',
}
STAGES = [
    ('Prepare Nix', 'Official Nix installer. Pinned version, verified integrity.'),
    ('Configure Nix', 'Enable nix-command and flakes through the configuration owner.'),
    ('Connect binary caches', 'Preserve existing caches and add the NeoHaskell caches.'),
    ('Download neo', 'Your command line for creating and running projects.'),
    ('Download neoctl', 'An independent NeoHaskell application. Help and version first.'),
    ('Check your setup', 'Check both tools, shell availability and effective cache use.'),
]


def label(text, color=WHITE, bold=False):
    return Text(text, style=('bold ' if bold else '') + color)


class Demo:
    def __init__(self, args):
        self.args = args
        self.variant = args.variant
        self.scenario = args.scenario
        self.plain = args.plain or not sys.stdout.isatty() or os.environ.get('TERM') == 'dumb'
        self.console = Console(no_color='NO_COLOR' in os.environ or self.plain,
                               color_system=None if self.plain else 'truecolor',
                               record=bool(args.snapshot), width=80 if args.snapshot else None)
        self.stage = 0
        self.detail = False
        self.retry = False
        self.tick = 0

    def key(self, timeout=None):
        if timeout is not None and not select.select([sys.stdin], [], [], timeout)[0]:
            return ''
        key = os.read(sys.stdin.fileno(), 1).decode(errors='ignore').lower()
        if key == '\x03':
            raise KeyboardInterrupt
        return key

    def title(self, index):
        return 'Keep your existing Nix' if index == 0 and self.scenario == 'existing' else STAGES[index][0]

    def header(self):
        brand = Text.assemble((' neo ', 'bold #21142c on ' + PINK), ('  NeoHaskell', 'bold ' + WHITE))
        return Group(brand, label('Install neo and neoctl.', MUTED), Text(''))

    def card(self, content, title=None, color=LILAC):
        if self.plain:
            return Group(label(title or ''), content)
        return Panel(content, title=title, title_align='left', box=box.ROUNDED,
                     border_style=color, style=WHITE + ' on ' + SURFACE, padding=(1, 3),
                     width=min(76, self.console.width - 2))

    def footer(self, text):
        return label(text, MUTED)

    def menu(self):
        if not self.plain:
            self.console.clear()
        rows = Table.grid(padding=(0, 3))
        rows.add_column()
        rows.add_column()
        descriptions = ['A compact, permanent transcript.', 'A focused live step, with a lasting history.', 'Every step visible in one live panel.']
        for (key, title), description in zip(VARIANTS.items(), descriptions):
            rows.add_row(label('[' + key + '] ' + title, PINK if key == self.variant else MUTED, key == self.variant), label(description, MUTED))
        self.console.print(Padding(self.card(Group(self.header(), label('Choose a direction', WHITE, True), Text(''), rows,
            Text(''), label(SCENARIOS[self.scenario], LILAC), Text(''),
            self.footer('1/2/3 design    S scenario    Enter preview    Q quit')), 'Prototype'), (1, 2)))
        self.console.print('    Simulation only. No installation, network calls or system changes.', style=MUTED)
        while True:
            key = self.key()
            if key in VARIANTS:
                self.variant = key
                return 'menu'
            if key == 's':
                scenarios = list(SCENARIOS)
                self.scenario = scenarios[(scenarios.index(self.scenario) + 1) % len(scenarios)]
                return 'menu'
            if key in ('\n', '\r'):
                return 'plan'
            if key == 'q':
                return 'exit'

    def plan_view(self):
        nix = 'Keep your compatible Nix installation.' if self.scenario == 'existing' else 'Install official Nix as a system service.'
        rows = Table.grid(padding=(0, 2))
        for number, title, detail in [('1', 'Prepare your environment', nix),
                                      ('2', 'Configure fast downloads', 'Set up Nix features and both binary caches.'),
                                      ('3', 'Install your tools', 'Download and check neo and neoctl.')]:
            rows.add_row(label(number, PINK, True), Group(label(title, WHITE, True), label(detail, MUTED), Text('')))
        return self.card(Group(self.header(), label('Make yourself at home.', WHITE, True), Text(''), rows,
            label('Administrator access', LILAC, True),
            label('Nix needs system-level changes. Your system will ask for', MUTED),
            label('your password when needed. This preview never asks for it.', MUTED), Text(''),
            Text.assemble((' Enter ', 'bold #21142c on ' + PINK), (' Continue    D Details    Q Cancel', MUTED))),
            'Your installation')

    def details(self):
        self.console.print('Configure through the real Nix owner. Preserve unrelated settings.')
        self.console.print('Caches. https://cache.iog.io and https://neohaskell.cachix.org')
        self.console.print('Verify signatures. Never expand trusted-users or update existing Nix.')

    def plan(self, automatic=False):
        if not self.plain:
            self.console.clear()
        blocked = {
            'systemd': ('WSL2 needs active systemd.', 'Enable systemd in your distribution, restart it from Windows, then run the installer again.'),
            'managed': ('Cannot locate the nix-darwin configuration source.', 'Ask the administrator for access to the declarative source, then run the installer again.'),
        }
        if self.scenario in blocked:
            title, recovery = blocked[self.scenario]
            self.console.print(self.card(Group(label(title, PINK, True), Text(''), label(recovery), Text(''),
                label('Nothing changed. Neither tool was installed.', MUTED)), 'Setup needs attention', PINK))
            return 'blocked'
        if self.plain:
            self.console.print('PROTOTYPE. All results are simulated. No system changes.')
            self.console.print('Keep compatible Nix.' if self.scenario == 'existing' else 'Install official Nix.')
            self.console.print('Configure Nix and both caches. Install neo and neoctl.')
        else:
            self.console.print(Padding(self.plan_view(), (1, 2)))
            self.console.print('    Simulation. All steps, byte counts and timing are fictional.', style=MUTED)
        if automatic:
            return 'running'
        while True:
            key = self.key()
            if key in ('\r', '\n'):
                return 'running'
            if key == 'q':
                return 'cancelled'
            if key == 'd':
                self.details()

    def activity(self):
        download = self.stage in (3, 4)
        amount = min(48, self.tick * 4)
        content = [label(self.title(self.stage), WHITE, True), label(STAGES[self.stage][1], MUTED), Text('')]
        if download:
            transfer = Table.grid(expand=True)
            transfer.add_row(ProgressBar(total=48, completed=amount, width=None, complete_style=LILAC,
                                        finished_style=MINT, style='#3e344f'))
            transfer.add_row(label(str(amount) + ' / 48 MiB   simulated transfer', LILAC))
            content += [transfer]
        else:
            waiting = Table.grid(expand=True)
            waiting.add_row(ProgressBar(total=None, pulse=True, pulse_style=LILAC, style='#3e344f'))
            waiting.add_row(label('Working. This step does not report a percentage.', MUTED))
            content += [waiting]
        if self.detail:
            content += [Text(''), label('Simulated event stream. No Nix commands are executed.', MUTED)]
        return Group(*content)

    def dashboard(self):
        rows = Table.grid(padding=(0, 3))
        for index in range(len(STAGES)):
            state = 'DONE' if index < self.stage else 'NOW' if index == self.stage else 'WAIT'
            rows.add_row(label(state, MINT if state == 'DONE' else PINK if state == 'NOW' else MUTED),
                         label(self.title(index), WHITE if index <= self.stage else MUTED, index == self.stage))
        return self.card(Group(self.header(), rows, Text(''), self.activity(), Text(''),
            self.footer('D details    Q cancel    Simulated installation')), 'Your environment')

    def run(self, automatic=False):
        for index in range(self.stage, len(STAGES)):
            self.stage = index
            self.tick = 0
            animated = not (automatic or self.plain or self.args.still or self.variant == '1')
            if animated:
                view = self.dashboard if self.variant == '3' else lambda: self.card(self.activity(), 'Step ' + str(index + 1) + ' of 6')
                with Live(view(), console=self.console, auto_refresh=False, transient=True, screen=self.variant == '3') as live:
                    for tick in range(13):
                        self.tick = tick
                        live.update(view(), refresh=True)
                        key = self.key(0.12)
                        if key == 'q':
                            return 'cancelled'
                        if key == 'd':
                            self.detail = not self.detail
            else:
                self.console.print('Working. ' + self.title(index), style=None if self.plain else LILAC)
                if not automatic:
                    key = self.key(0.7)
                    if key == 'q':
                        return 'cancelled'
                    if key == 'd':
                        self.details()
            if self.scenario == 'network' and index == 4 and not self.retry:
                self.console.print(self.card(Group(label('neoctl download interrupted.', PINK, True), Text(''),
                    label('Your connection dropped. Setup is not complete.'),
                    label('Nix and caches are configured. neo is downloaded.', MUTED), Text(''),
                    label('Check your connection, then retry.', WHITE, True),
                    self.footer('R retry simulation    Q quit')), 'Let\'s get you unstuck', PINK))
                return 'failed'
            self.console.print(Text.assemble(('  DONE  ', MINT if not self.plain else ''), (self.title(index), '')))
        return 'complete'

    def result(self, state):
        if state == 'complete':
            if self.plain:
                self.console.print('NeoHaskell is ready. neo and neoctl are available.')
                self.console.print('Next. neo new my-app')
            else:
                self.console.print(Padding(self.card(Group(label('You\'re ready to build.', MINT, True), Text(''),
                    label('neo and neoctl are available.'), label('Nix and binary caches checked.', MUTED), Text(''),
                    label('Create your first project', WHITE, True),
                    Text(' neo new my-app ', style='bold #21142c on ' + MINT)), 'All set', MINT), (1, 2)))
            self.console.print('Simulated result. No tools were installed.', style=None if self.plain else MUTED)
        if state == 'cancelled':
            self.console.print('Installation cancelled. This simulation changed nothing.')
            self.console.print('The real installer must report any changes already applied.')


def main():
    parser = argparse.ArgumentParser(description='Disposable terminal prototype. No system changes.')
    parser.add_argument('--variant', choices=VARIANTS, default='2')
    parser.add_argument('--scenario', choices=SCENARIOS, default='new')
    parser.add_argument('--plain', action='store_true')
    parser.add_argument('--still', action='store_true', help='Disable motion and live updates.')
    parser.add_argument('--yes', action='store_true', help='Accept the simulation without prompting.')
    parser.add_argument('--snapshot', choices=['plan', 'running', 'complete'])
    args = parser.parse_args()
    demo = Demo(args)
    if args.snapshot:
        demo.plain = False
        demo.stage, demo.tick = 3, 7
        if args.snapshot == 'complete':
            demo.result('complete')
        else:
            demo.console.print(demo.plan_view() if args.snapshot == 'plan' else demo.dashboard() if args.variant == '3' else demo.card(demo.activity(), 'Installing'))
        path = Path(__file__).parent / 'captures' / ('studio-' + args.variant + '-' + args.snapshot + '.svg')
        path.parent.mkdir(exist_ok=True)
        demo.console.save_svg(str(path), title='NeoHaskell installer prototype')
        return 0
    if args.yes or not sys.stdin.isatty():
        if not args.yes:
            demo.console.print('No interactive terminal. Use --yes to accept the simulation.')
            return 2
        state = demo.plan(automatic=True)
        if state == 'running':
            state = demo.run(automatic=True)
        demo.result(state)
        return 0 if state == 'complete' else 1
    settings = termios.tcgetattr(sys.stdin)
    try:
        tty.setcbreak(sys.stdin.fileno())
        state = 'menu'
        while state != 'exit':
            if state == 'menu':
                state = demo.menu()
            elif state == 'plan':
                demo.stage, demo.retry, demo.detail = 0, False, False
                state = demo.plan()
                if state == 'cancelled':
                    demo.result(state)
            elif state == 'running':
                state = demo.run()
                demo.result(state)
            else:
                demo.console.print('M compare designs    Q quit', style=MUTED)
                key = demo.key()
                if key == 'r' and state == 'failed':
                    demo.retry = True
                    state = 'running'
                elif key == 'm':
                    state = 'menu'
                elif key == 'q':
                    state = 'exit'
    except KeyboardInterrupt:
        demo.result('cancelled')
        return 130
    finally:
        termios.tcsetattr(sys.stdin, termios.TCSADRAIN, settings)
    return 0


if __name__ == '__main__':
    sys.exit(main())
