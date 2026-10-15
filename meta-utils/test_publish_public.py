"""Публичная история не должна наследовать коммиты приватного эталона."""

import subprocess
import tempfile
import unittest
from pathlib import Path

from publish_public import git, publish


class PublicSnapshotTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        root = Path(self.tmp.name)
        self.private = root / "private"
        self.public = root / "public.git"
        git("init", "-b", "main", str(self.private))
        git("-C", str(self.private), "config", "user.name", "Test")
        git("-C", str(self.private), "config", "user.email", "test@example.invalid")
        git("init", "--bare", str(self.public))
        git("remote", "add", "public", str(self.public), cwd=self.private)
        (self.private / "answer.txt").write_text("private solution\n")
        self.commit("Private reference")
        self.secret = git("rev-parse", "HEAD", cwd=self.private)
        (self.private / "answer.txt").unlink()
        (self.private / "task.txt").write_text("todo\n")
        self.commit("Student starter")

    def commit(self, message):
        git("add", "-A", cwd=self.private)
        git("commit", "-m", message, cwd=self.private)

    def test_first_snapshot_has_no_private_ancestors(self):
        snapshot = publish("Выдать ДЗ", cwd=self.private)
        self.assertEqual(git("rev-list", "--all", cwd=self.public), snapshot)
        self.assertEqual(git("show", "main:task.txt", cwd=self.public), "todo")
        missing = subprocess.run(
            ["git", "cat-file", "-e", self.secret], cwd=self.public,
            stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
        )
        self.assertNotEqual(missing.returncode, 0)

    def test_updates_keep_only_public_ancestry_and_exact_tree(self):
        first = publish("First", cwd=self.private)
        (self.private / "task.txt").write_text("updated todo\n")
        (self.private / "README.md").write_text("Условия\n")
        self.commit("Updated starter")
        second = publish("Second", cwd=self.private)
        self.assertEqual(git("rev-list", "main", cwd=self.public).splitlines(), [second, first])
        self.assertEqual(git("rev-parse", "main^{tree}", cwd=self.public),
                         git("rev-parse", "main^{tree}", cwd=self.private))

    def test_same_tree_is_noop(self):
        first = publish("First", cwd=self.private)
        self.assertEqual(publish("Again", cwd=self.private), first)
        self.assertEqual(git("rev-list", "--count", "main", cwd=self.public), "1")


if __name__ == "__main__":
    unittest.main()
