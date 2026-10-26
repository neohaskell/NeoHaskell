import importlib.util
from pathlib import Path
import sys
import tempfile
import unittest

sys.dont_write_bytecode = True
spec = importlib.util.spec_from_file_location('resource_check', Path(__file__).resolve().parents[1] / 'scripts/check.py')
check = importlib.util.module_from_spec(spec)
spec.loader.exec_module(check)


class ResourceTests(unittest.TestCase):
    def setUp(self):
        root = Path(__file__).resolve()
        while not (root / '.git').exists():
            if root.parent == root:
                raise RuntimeError('Tests need a repository')
            root = root.parent
        directory = tempfile.TemporaryDirectory()
        self.addCleanup(directory.cleanup)
        self.root = Path(directory.name).resolve()

    def skill(self, name, body='', metadata=None):
        path = self.root / '.agents/skills' / name / 'SKILL.md'
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(metadata or f'---\nname: {name}\ndescription: Validate a concrete target.\n---\n\n{body}')
        return path

    def test_cross_owner_link_resolves_and_missing_resource_is_reported(self):
        first = self.skill('first', '[shared](../second/references/shared.md)')
        second = self.skill('second')
        shared = second.parent / 'references/shared.md'
        shared.parent.mkdir()
        shared.write_text('owned reference')
        self.assertEqual(check.validate(self.root)['errors'], [])
        shared.unlink()
        self.assertIn('missing link', '\n'.join(check.validate(self.root)['errors']))

    def test_fenced_examples_and_external_urls_are_not_filesystem_dependencies(self):
        self.skill('example', '[external](https://example.com/reference)\n```markdown\n[example](not-a-real-file.md)\n```\n')
        self.assertEqual(check.validate(self.root)['errors'], [])

    def test_template_link_placeholders_are_skipped_but_real_missing_links_fail(self):
        skill = self.skill('example')
        references = skill.parent / 'references'
        references.mkdir()
        (references / 'template.md').write_text(
            '[source](url)\n[angle template](<...>)\n[curly template]({...})\n[missing](missing.md)\n'
        )
        errors = check.validate(self.root)['errors']
        self.assertEqual(len(errors), 1)
        self.assertIn('missing link missing.md', errors[0])

    def test_plain_yaml_nonstring_scalars_fail_while_quoted_values_remain_strings(self):
        for raw in ('null', '~', 'true', 'yes', '12', '1.5', '1e3', '0x10', '.inf', '-.inf', '1:20', '1:20.5', '+1:20', '01:20', '0:20', '1:20:30.5', '2025-03-08'):
            with self.subTest(raw=raw):
                fields, errors = check.parse_frontmatter(
                    f'---\nname: example\ndescription: {raw}\n---\n'
                )
                self.assertTrue(errors)
                self.assertNotIn('description', fields)
        for raw in ('"null"', "'true'", '"12"', '"1:20"', "'1:20.5'", '"1:20:30.5"', '"1:60"'):
            with self.subTest(raw=raw):
                fields, errors = check.parse_frontmatter(
                    f'---\nname: example\ndescription: {raw}\n---\n'
                )
                self.assertEqual(errors, [])
                self.assertEqual(fields['description'], raw[1:-1])

    def test_invalid_sexagesimal_numeric_ranges_remain_plain_strings(self):
        for raw in ('1:60', '1:60.5'):
            with self.subTest(raw=raw):
                fields, errors = check.parse_frontmatter(
                    f'---\nname: example\ndescription: {raw}\n---\n'
                )
                self.assertEqual(errors, [])
                self.assertEqual(fields['description'], raw)

    def test_duplicate_name_and_blank_description_are_errors(self):
        self.skill('first')
        self.skill('second', metadata='---\nname: first\ndescription: ""\n---\n')
        errors = '\n'.join(check.validate(self.root)['errors'])
        self.assertIn('duplicate name', errors)
        self.assertIn('description is empty', errors)

    def test_folded_description_and_optional_metadata_are_supported(self):
        self.skill('example', metadata='---\nname: example\ndescription: >\n  Check one concrete\n  contract.\nmetadata:\n  category: development\n---\n')
        self.assertEqual(check.validate(self.root)['errors'], [])

    def test_single_skill_does_not_enforce_an_unrelated_collection(self):
        path = self.skill('example')
        common = self.root / '.agents/workflow/scripts/helper.py'
        common.parent.mkdir(parents=True)
        common.write_text('unrelated')
        self.assertEqual(check.validate(self.root, skill=path.parent)['errors'], [])
        self.assertIn('move skill resources', '\n'.join(check.validate(self.root)['errors']))

    def test_installed_dependency_documents_are_excluded_in_all_selection_modes(self):
        skill = self.skill('example')
        dependency = skill.parent / 'scripts/node_modules/package/node_modules/nested/README.md'
        dependency.parent.mkdir(parents=True)
        dependency.write_text('[upstream](missing.md)')
        for selection in ({}, {'skill': skill.parent}, {'skills_root': skill.parent.parent}):
            with self.subTest(selection=selection):
                result = check.validate(self.root, **selection)
                self.assertEqual(result['errors'], [])
                self.assertEqual(result['documents'], 1)

    def test_untracked_authored_document_next_to_dependencies_is_still_checked(self):
        skill = self.skill('example')
        scripts = skill.parent / 'scripts'
        dependency = scripts / 'node_modules/package/README.md'
        dependency.parent.mkdir(parents=True)
        dependency.write_text('[upstream](missing-upstream.md)')
        authored = scripts / 'usage.md'
        authored.write_text('[owned](missing-owned.md)')
        result = check.validate(self.root)
        self.assertEqual(result['documents'], 2)
        self.assertEqual(result['errors'], [f'{authored}: missing link missing-owned.md'])

    def test_authored_links_into_missing_dependency_resources_still_fail(self):
        skill = self.skill('example', '[dependency](scripts/node_modules/package/missing.md)')
        self.assertEqual(check.validate(self.root)['errors'], [
            f'{skill}: missing link scripts/node_modules/package/missing.md'
        ])

    def test_external_git_free_skills_exclude_only_installed_dependency_documents(self):
        skill = self.skill('example')
        external = self.root / 'external'
        skill.parent.parent.rename(external)
        skill = external / 'example/SKILL.md'
        dependency = skill.parent / 'node_modules/package/README.md'
        dependency.parent.mkdir(parents=True)
        dependency.write_text('[upstream](missing.md)')
        for selection in ({'skill': skill.parent}, {'skills_root': external}):
            with self.subTest(selection=selection):
                result = check.validate(self.root, **selection)
                self.assertEqual(result['errors'], [])
                self.assertEqual(result['documents'], 1)
        authored = skill.parent / 'usage.md'
        authored.write_text('[owned](missing-owned.md)')
        for selection in ({'skill': skill.parent}, {'skills_root': external}):
            with self.subTest(selection=selection):
                self.assertEqual(check.validate(self.root, **selection)['errors'], [
                    f'{authored}: missing link missing-owned.md'
                ])

    def test_similarly_named_authored_directory_is_still_checked(self):
        skill = self.skill('example')
        authored = skill.parent / 'node_modules-notes/usage.md'
        authored.parent.mkdir()
        authored.write_text('[owned](missing.md)')
        result = check.validate(self.root)
        self.assertEqual(result['documents'], 2)
        self.assertEqual(result['errors'], [f'{authored}: missing link missing.md'])


if __name__ == '__main__':
    unittest.main()
