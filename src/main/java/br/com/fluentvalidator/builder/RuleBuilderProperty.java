package br.com.fluentvalidator.builder;

import br.com.fluentvalidator.rule.Rule;

public interface RuleBuilderProperty<T, P> extends RuleBuilder<T, P, WhenProperty<T, P>>, Rule<T> {
		
}
