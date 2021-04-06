package br.com.fluentvalidator.builder;

public interface Critical<T, P, W extends When<T, P, W, N>, N extends Whenever<T, P, W, N>> extends RuleBuilder<T, P, W, N> {

}
