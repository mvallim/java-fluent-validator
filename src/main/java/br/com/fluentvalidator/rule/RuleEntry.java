package br.com.fluentvalidator.rule;

import java.util.function.Predicate;

import br.com.fluentvalidator.builder.Rule;

class RuleEntry<W, P> {

	private final Predicate<W> when;

	private final Rule<P> rule;

	public RuleEntry(final Predicate<W> when, final Rule<P> rule) {
		this.when = when;
		this.rule = rule;
	}

	public Predicate<W> getWhen() {
		return when;
	}

	public Rule<P> getRule() {
		return rule;
	}

}