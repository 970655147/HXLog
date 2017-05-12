package com.hx.log.interf;

import java.io.File;

/**
 * 消费文件的消费者
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/12/2017 10:06 PM
 */
public interface FileConsumer<T> extends Consumer<File, T> {

}
