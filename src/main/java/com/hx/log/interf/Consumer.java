package com.hx.log.interf;

/**
 * 消费者的抽象
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/12/2017 10:07 PM
 */
public interface Consumer<I, O> {

    /**
     * 消耗给定的输入
     *
     * @param content 给定的内容
     * @return void
     * @author Jerry.X.He
     * @date 5/12/2017 9:11 PM
     * @since 1.0
     */
    void consume(I content);

    /**
     * 获取当前消费者的结果
     *
     * @return T the result of consumer
     * @author Jerry.X.He
     * @date 5/12/2017 9:11 PM
     * @since 1.0
     */
    O get();

}
