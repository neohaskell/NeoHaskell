import importlib.util
import hashlib
import json
from pathlib import Path
import tempfile
import unittest

spec = importlib.util.spec_from_file_location('measure', Path(__file__).resolve().parents[2]/'docs/build-cache/measure.py')
m = importlib.util.module_from_spec(spec)
spec.loader.exec_module(m)


class Measurements(unittest.TestCase):
    def test_no_small_sample_median_or_mixed_cache_states(self):
        with tempfile.TemporaryDirectory() as directory:
            paths=[]
            for i in range(3):
                path=Path(directory)/str(i);path.mkdir();paths.append(path)
                entry={'revision':'a'*40,'system':'linux','scenario':'repeat','stage':'build',
                       'local_state':'warm','remote_state':'known','lock_sha256':'b'*64,
                       'command':['build'],'elapsed_s':[2,9,4][i],'exit_code':0,'logs':{}}
                (path/'observation.json').write_text(json.dumps(entry))
            self.assertIsNone(m.summarize(paths[:2])[0]['median_s'])
            self.assertEqual(m.summarize(paths)[0]['median_s'],4)
            entry['local_state']='empty';(paths[-1]/'observation.json').write_text(json.dumps(entry))
            groups=m.summarize(paths)
            self.assertEqual(len(groups),2)
            self.assertTrue(all(group['median_s'] is None for group in groups))

    def test_failed_samples_and_changed_logs_are_not_success_evidence(self):
        with tempfile.TemporaryDirectory() as directory:
            path=Path(directory);log=path/'command.log';log.write_text('failed')
            entry={'revision':'a'*40,'system':'linux','scenario':'repeat','stage':'build',
                   'local_state':'warm','remote_state':'known','lock_sha256':'b'*64,
                   'command':['build'],'elapsed_s':2,'exit_code':1,
                   'logs':{'command.log':hashlib.sha256(log.read_bytes()).hexdigest()}}
            (path/'observation.json').write_text(json.dumps(entry))
            self.assertIsNone(m.summarize([path])[0]['median_s'])
            with self.assertRaisesRegex(ValueError,'duplicate observation'):m.summarize([path,path,path])
            log.write_text('success')
            with self.assertRaisesRegex(ValueError,'digest mismatch'):m.summarize([path])

if __name__=='__main__':unittest.main()
