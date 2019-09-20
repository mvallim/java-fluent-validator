package br.com.fluentvalidator.builder;

import java.util.Collection;

public interface RuleBuilderCollection<T, P>
    extends RuleBuilder<T, Collection<P>, WhenCollection<T, P>> {

}
