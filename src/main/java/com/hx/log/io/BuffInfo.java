package com.hx.log.io;

import com.hx.log.io.interf.BuffSizeEstimator;
import com.hx.log.io.interf.BufferHandler;

/**
 * BuffInfo
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/5/2017 12:16 AM
 */
public class BuffInfo {

    /**
     * 输出文件的路径
     */
    private String outputPath;
    /**
     * 输出的编码
     */
    private String charset;
    /**
     * 缓冲输出的阈值
     */
    private int threshold;
    /**
     * 缓冲的大小
     */
    private int buffSize;
    /**
     * 缓冲数据
     */
    private StringBuffer sb;
    /**
     * buffHandler
     */
    private BufferHandler handler;

    /**
     * 初始化
     *
     * @param output * @param outputPath
 * @param charset
 * @param threshold
 * @param buffSizeEstimator
 * @param handler @param buffSizeEstimator buffSizeEstimator
     * @param handler           buffHandler
     * @return
     * @author
     * @date
     * @since 1.0
     */
    public BuffInfo(String outputPath, String charset, int threshold,
                    BuffSizeEstimator buffSizeEstimator, BufferHandler handler) {
        this.outputPath = outputPath;
        this.charset = charset;
        this.threshold = threshold;
        this.buffSize = buffSizeEstimator.getBuffSize(threshold);
        this.handler = handler;
        this.sb = new StringBuffer(buffSize);
    }

    /**
     * setter & getter
     */
    public String getOutputPath() {
        return outputPath;
    }

    public String getCharset() {
        return charset;
    }

    public int getThreshold() {
        return threshold;
    }

    public int getBuffSize() {
        return buffSize;
    }

    public StringBuffer getSb() {
        return sb;
    }

    public BufferHandler getHandler() {
        return handler;
    }
}
