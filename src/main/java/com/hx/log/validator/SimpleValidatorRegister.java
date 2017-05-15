package com.hx.log.validator;

import com.hx.common.interf.common.Result;
import com.hx.log.util.Tools;
import com.hx.common.interf.validator.ValidateContext;
import com.hx.common.interf.validator.Validator;
import com.hx.common.interf.validator.ValidatorRegister;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * SimpleValidatorRegister
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/3/2017 10:16 PM
 */
public class SimpleValidatorRegister implements ValidatorRegister {

    /**
     * 需要校验的数据 + 校验器 + 额外数据 组成的List
     */
    private List<ValidateContextTriple> contextTripleList;

    /**
     * 初始化
     *
     * @author Jerry.X.He <970655147@qq.com>
     * @date 5/3/2017 10:16 PM
     * @since 1.0
     */
    public SimpleValidatorRegister() {
        contextTripleList = new ArrayList<>();
    }

    @Override
    public ValidatorRegister register(Object obj, Validator validator, Object extra) {
        Tools.assert0(validator != null, "'validator' can't be null !");
        contextTripleList.add(new ValidateContextTriple(obj, validator, extra));
        return this;
    }

    @Override
    public ValidatorRegister register(Object obj, Validator validator) {
        return register(obj, validator, null);
    }

    @Override
    public Iterator<ValidateContext> iterator() {
        return new ValidateContextTripleIterator(contextTripleList.iterator() );
    }

    @Override
    public Result apply() {
        for (ValidateContextTriple triple : contextTripleList) {
            Result result = triple.validator.validate(triple.obj, triple.extra);
            if (!result.success()) {
                return result;
            }
        }

        return ValidateResultUtils.success();
    }

    /**
     * 校验上下文的三元组
     *
     * @author Jerry.X.He <970655147@qq.com>
     * @version 1.0
     * @date 5/3/2017 10:18 PM
     */
    private static class ValidateContextTriple implements ValidateContext {
        /**
         * 给定的需要校验的对象
         */
        public Object obj;
        /**
         * validator
         */
        public Validator validator;
        /**
         * 校验的时候 需要传入的额外的对象
         */
        public Object extra;

        /**
         * ValidateContextTriple
         *
         * @param obj       给定的对象
         * @param validator 给定的validator
         * @param extra     额外的信息
         * @author Jerry.X.He <970655147@qq.com>
         * @date 5/3/2017 10:18 PM
         * @since 1.0
         */
        public ValidateContextTriple(Object obj, Validator validator, Object extra) {
            this.obj = obj;
            this.validator = validator;
            this.extra = extra;
        }

        @Override
        public Object obj() {
            return null;
        }

        @Override
        public Validator validator() {
            return null;
        }

        @Override
        public Object extra() {
            return null;
        }
    }

    /**
     * ValidateContextTripleIterator
     *
     * @author Jerry.X.He <970655147@qq.com>
     * @version 1.0
     * @date 5/3/2017 10:27 PM
     */
    private static class ValidateContextTripleIterator implements Iterator<ValidateContext> {
        /**
         * 组合的iterator
         */
        private Iterator<ValidateContextTriple> ite;

        /**
         * 初始化
         *
         * @author Jerry.X.He <970655147@qq.com>
         * @date 5/3/2017 10:27 PM
         * @since 1.0
         */
        public ValidateContextTripleIterator(Iterator<ValidateContextTriple> ite) {
            this.ite = ite;
        }

        @Override
        public boolean hasNext() {
            return ite.hasNext();
        }

        @Override
        public ValidateContext next() {
            return ite.next();
        }

        @Override
        public void remove() {
            ite.remove();
        }
    }

}
