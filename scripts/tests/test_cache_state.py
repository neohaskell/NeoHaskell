import importlib.machinery
import importlib.util
import urllib.error
import unittest
from pathlib import Path
from unittest.mock import patch


ROOT = Path(__file__).resolve().parents[2]
loader = importlib.machinery.SourceFileLoader('cache_state', str(ROOT / 'docs/build-cache/cache-state.py'))
spec = importlib.util.spec_from_loader(loader.name, loader)
cache_state = importlib.util.module_from_spec(spec)
loader.exec_module(cache_state)


class CacheState(unittest.TestCase):
    def test_configured_caches_include_inherited_and_flake_endpoints(self):
        caches, config = cache_state.configured_caches({'substituters': {'value': [
            'https://cache.nixos.org/', 'https://install.determinate.systems',
            'https://cache.iog.io',
        ]}})
        self.assertEqual(set(caches), cache_state.PUBLIC_CACHES)
        self.assertEqual(config['unrecognized_count'], 0)

    def test_unknown_or_credentialed_cache_is_unusable_and_redacted(self):
        caches, config = cache_state.configured_caches({'substituters': {'value': [
            'https://token:secret@cache.nixos.org', 'https://unknown.example?token=secret',
        ]}})
        self.assertEqual(config['unrecognized_count'], 2)
        self.assertNotIn('secret', str((caches, config)))
        self.assertEqual(len(config['sha256']), 64)

    def test_narinfo_200_requires_the_requested_store_path(self):
        path = '/nix/store/' + 'a' * 32 + '-component'

        class Response:
            def __enter__(self):
                return self

            def __exit__(self, *_):
                return False

            def getcode(self):
                return 200

            def read(self, limit):
                self.limit = limit
                return f'StorePath: {path}\n'.encode()

        with patch.object(cache_state.urllib.request, 'urlopen', return_value=Response()) as opener:
            result = cache_state.narinfo('https://cache.example', path)
        self.assertEqual(result['state'], 'present')
        self.assertEqual(opener.call_args.kwargs['timeout'], cache_state.TIMEOUT_S)

    def test_narinfo_404_is_absent(self):
        path = '/nix/store/' + 'b' * 32 + '-component'
        error = urllib.error.HTTPError('https://cache.example', 404, 'missing', {}, None)
        with patch.object(cache_state.urllib.request, 'urlopen', side_effect=error):
            self.assertEqual(cache_state.narinfo('https://cache.example', path)['state'], 'absent')

    def test_narinfo_transport_error_is_unusable(self):
        path = '/nix/store/' + 'c' * 32 + '-component'
        with patch.object(cache_state.urllib.request, 'urlopen',
                          side_effect=urllib.error.URLError('offline')):
            result = cache_state.narinfo('https://cache.example', path)
        self.assertEqual(result['state'], 'network-error')
        self.assertIn('offline', result['error'])

    def test_expression_contains_bundle_and_all_named_components(self):
        expression = cache_state.expression()
        for name in ('ci-components',) + cache_state.COMPONENTS:
            self.assertIn(f'"{name}"', expression)


if __name__ == '__main__':
    unittest.main()
