module Extension = {
  external getURL : string => string = "chrome.extension.getURL" [@@bs.val];
};

module Runtime = {
  external sendMessage : 'a => ('b => unit) = "chrome.runtime.sendMessage" [@@bs.val];
  external addMessageListener : ('a => Js.t {..} => ('b => unit) => unit) => unit = "chrome.runtime.onMessage.addListener" [@@bs.val];
};

module Storage = {
  module Local = {
    external get : string => ('a => unit) => unit = "chrome.storage.local.get" [@@bs.val];
    external set : Js.t {..} => unit = "chrome.storage.local.set" [@@bs.val];
  }
};

module Tabs = {
  type tabId;
  /*
  type tab = Js.t {.
    id: tabId
  };
  */

  external create : Js.t {. url: string } => unit = "chrome.tabs.create" [@@bs.val];

  /* TODO: Need MaybeArray to work because Chrome will return an array, but FF supposedly does not */
  /*external executeScript : Js.t {. code: string } => (MaybeArray.t (Js.t {..}) => unit) => unit = "chrome.tabs.executeScript" [@@bs.val];*/
  external executeScript : Js.t {. code: string } => (Js.null_undefined (array string) => unit) => unit = "chrome.tabs.executeScript" [@@bs.val];
  external executeScriptFile : Js.t {. file: string } => (unit => unit) => unit = "chrome.tabs.executeScript" [@@bs.val];
  /* TODO: could use bs.ginore here? */

  external sendMessage : tabId => 'a => ('b => unit) = "chrome.tabs.sendMessage" [@@bs.val];
};

module ContextMenus = {
  /*
  type config = Js.t {.
    title: string,
    context: array string,
    onclick: (unit => Tabs.tab => unit)
  };
  */

  external create : /*config*/ Js.t {..} => unit = "chrome.contextMenus.create" [@@bs.val];
};
