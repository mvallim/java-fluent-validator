package br.com.fluentvalidator.rule;

import java.util.Collection;

import br.com.fluentvalidator.builder.Rule;

final class ValidationProcessor {

	private ValidationProcessor() {
		super();
	}
	
	public static <P> boolean process(final P value, final Rule<P> rule) {
		return rule.apply(value);
	}
	
	public static <P> boolean process(final Collection<P> values, final Rule<P> rule) {
		for (final P value : values) {
			if (!rule.apply(value)) return false;
		}
		return true;
	}


}
