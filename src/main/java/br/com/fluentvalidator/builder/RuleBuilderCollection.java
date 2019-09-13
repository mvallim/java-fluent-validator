package br.com.fluentvalidator.builder;

import java.util.Collection;

import br.com.fluentvalidator.rule.Rule;

public interface RuleBuilderCollection<T, P> extends RuleBuilder<T, Collection<P>, WhenCollection<T, P>>, Rule<T> {
		
}
