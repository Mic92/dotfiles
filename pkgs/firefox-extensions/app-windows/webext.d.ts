// Minimal WebExtension API surface used by this extension.

interface BrowserTab {
  id?: number;
  windowId?: number;
  url?: string;
  index: number;
}

interface BrowserWindow {
  id?: number;
  focused: boolean;
}

interface TabChangeInfo {
  url?: string;
}

type Listener<F> = {
  addListener(cb: F): void;
  removeListener(cb: F): void;
};

declare const browser: {
  storage: {
    sync: {
      get<T>(defaults: T): Promise<T>;
      set(items: object): Promise<void>;
    };
  };
  sessions: {
    setWindowValue(
      windowId: number,
      key: string,
      value: unknown,
    ): Promise<void>;
    getWindowValue(windowId: number, key: string): Promise<unknown>;
    setTabValue(tabId: number, key: string, value: unknown): Promise<void>;
    getTabValue(tabId: number, key: string): Promise<unknown>;
  };
  windows: {
    WINDOW_ID_NONE: number;
    get(id: number): Promise<BrowserWindow>;
    getAll(
      opts?: { populate?: boolean; windowTypes?: string[] },
    ): Promise<BrowserWindow[]>;
    create(opts: { url?: string; type?: string }): Promise<BrowserWindow>;
    update(id: number, opts: { focused?: boolean }): Promise<BrowserWindow>;
    onFocusChanged: Listener<(windowId: number) => void>;
    onRemoved: Listener<(windowId: number) => void>;
  };
  contextualIdentities: {
    query(
      opts: { name?: string },
    ): Promise<{ cookieStoreId: string; name: string }[]>;
    create(
      opts: { name: string; color: string; icon: string },
    ): Promise<{ cookieStoreId: string }>;
  };
  tabs: {
    get(id: number): Promise<BrowserTab>;
    query(opts: { windowId?: number; index?: number }): Promise<BrowserTab[]>;
    create(opts: {
      windowId?: number;
      url?: string;
      active?: boolean;
      cookieStoreId?: string;
      pinned?: boolean;
    }): Promise<BrowserTab>;
    update(id: number, opts: { active?: boolean }): Promise<BrowserTab>;
    move(
      id: number,
      opts: { windowId?: number; index: number },
    ): Promise<BrowserTab | BrowserTab[]>;
    remove(id: number): Promise<void>;
    onCreated: Listener<(tab: BrowserTab) => void>;
    onUpdated: Listener<
      (tabId: number, changeInfo: TabChangeInfo, tab: BrowserTab) => void
    >;
    onRemoved: Listener<(tabId: number) => void>;
  };
  browserAction: {
    onClicked: Listener<() => void>;
  };
  runtime: {
    onMessage: Listener<
      (
        msg: { type: string; [k: string]: unknown },
        sender: { tab?: BrowserTab },
      ) => unknown
    >;
    sendMessage(msg: object): Promise<unknown>;
  };
};
