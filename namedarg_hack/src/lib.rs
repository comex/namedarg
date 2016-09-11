#![feature(plugin_registrar, rustc_private)]

extern crate rustc;
extern crate rustc_plugin;
extern crate syntax;

use rustc::lint::{EarlyContext, LintPass, EarlyLintPass, LintArray};
use syntax::{ast, attr};
use rustc_plugin::Registry;

struct Pass;

impl LintPass for Pass {
    fn get_lints(&self) -> LintArray { &[] }
}

impl EarlyLintPass for Pass {
    fn check_item(&mut self, _: &EarlyContext, item: &ast::Item) {
        if attr::contains_name(&item.attrs, "rustc_derive_registrar") {
            // undefined behavior is fun
            let evil_item = unsafe {
                &mut *(item as *const ast::Item as *mut ast::Item)
            };
            if item.ident.name.as_str() == "hack_plugin_registrar" {
                evil_item.vis = ast::Visibility::Public;
            } else {
                evil_item.attrs.clear();
            }
        }
    }
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_early_lint_pass(Box::new(Pass));
}
