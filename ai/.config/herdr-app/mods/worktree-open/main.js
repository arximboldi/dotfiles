// Keyboard launcher for Herdr's worktree dialog ("open"): mod.open can't pass
// props, so this opens the shipped dialog for the active space and closes itself.
import { View, div } from "gpui-kit";
import { spaces, panes, openOverlay, closeOverlay, props } from "herdr";

export default class WorktreeLauncher extends View {
  init(_props, cx) {
    cx.spawn(async () => {
      const space = spaces().find((s) => s.active);
      closeOverlay(props().overlay);
      if (!space) return;
      openOverlay({ mod: "worktree", kind: "dialog", width: 560, height: 480,
        props: { space: space.id, folder: panes().find((p) => p.focused)?.cwd, action: "open" } });
    });
  }
  render() { return div(); }
}
