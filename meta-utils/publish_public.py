"""Публикация снимка условий без переноса приватной истории в публичный Git."""

import argparse
import subprocess


def git(*args: str, cwd=None, input=None) -> str:
    result = subprocess.run(
        ["git", *args], cwd=cwd, input=input, text=True,
        stdout=subprocess.PIPE, check=True,
    )
    return result.stdout.strip()


def publish(message: str, cwd=None) -> str:
    tree = git("rev-parse", "main^{tree}", cwd=cwd)
    remote = git("ls-remote", "public", "refs/heads/main", cwd=cwd)
    parents = []
    if remote:
        parent = remote.split()[0]
        git("fetch", "public", "refs/heads/main", cwd=cwd)
        if git("rev-parse", f"{parent}^{{tree}}", cwd=cwd) == tree:
            print("Публичные условия уже актуальны.")
            return parent
        parents = ["-p", parent]
    snapshot = git("commit-tree", tree, *parents, cwd=cwd, input=message + "\n")
    # Без --force: одновременное изменение публичной ветки останавливает публикацию.
    git("push", "public", f"{snapshot}:refs/heads/main", cwd=cwd)
    print(f"Публичный снимок условий: {snapshot}")
    return snapshot


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--message", default="Обновить условия домашнего задания")
    args = parser.parse_args()
    publish(args.message)
